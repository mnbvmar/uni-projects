#include <linux/printk.h>

#include "command.h"
#include "common.h"
#include "harddoom.h"

static u32 doomcmd_num_free_commands(struct harddoom_dev *dev)
{
	return ioread32(dev->bar_iomap + HARDDOOM_FIFO_FREE);
}

static int doomcmd_put_command_main(struct harddoom_dev *dev, u32 cmd)
{
	BUG_ON(!doomcmd_num_free_commands(dev));
	iowrite32(cmd, dev->bar_iomap + HARDDOOM_FIFO_SEND);
	++dev->writer.cmds_from_async;
	dev->writer.is_synchronized = false;
	return 0;
}

#define INJECT_ASYNC_EVERY 128

static int doomcmd_put_command(struct harddoom_dev *dev, u32 cmd)
{
	int err;
	// Inject async ping operation once in a while.
	if (dev->writer.cmds_from_async >= INJECT_ASYNC_EVERY) {
		// Needs to be set to 0 before the recursive call.
		// We don't want to branch here again.
		dev->writer.cmds_from_async = 0;
		if (unlikely(err = doomcmd_put_command(
				 dev, HARDDOOM_CMD_PING_ASYNC))) {
			// Revert the previously set value so that we will try
			// asyncing again.
			dev->writer.cmds_from_async = INJECT_ASYNC_EVERY;
			return err;
		}
	}

	if (likely(doomcmd_num_free_commands(dev)))
		return doomcmd_put_command_main(dev, cmd);

	// No space here. Wait for some space (async ping should do).
	doomcmd_clear_interrupts(dev, HARDDOOM_INTR_PONG_ASYNC);
	if (likely(doomcmd_num_free_commands(dev)))
		return doomcmd_put_command_main(dev, cmd);

	doomcmd_interrupts_on(dev, HARDDOOM_INTR_PONG_ASYNC);
	err = doomcmd_interrupts_await(dev, HARDDOOM_INTR_PONG_ASYNC);
	doomcmd_interrupts_off(dev, HARDDOOM_INTR_PONG_ASYNC);

	if (unlikely(err))
		return err;
	return doomcmd_put_command_main(dev, cmd);
}

void doomcmd_interrupts_on(struct harddoom_dev *dev, u32 intr_mask)
{
	__iomem void *intr_loc = dev->bar_iomap + HARDDOOM_INTR_ENABLE;
	u32 cur_intrs = ioread32(intr_loc);
	iowrite32(cur_intrs | intr_mask, intr_loc);
}

void doomcmd_interrupts_off(struct harddoom_dev *dev, u32 intr_mask)
{
	__iomem void *intr_loc = dev->bar_iomap + HARDDOOM_INTR_ENABLE;
	u32 cur_intrs = ioread32(intr_loc);
	iowrite32(cur_intrs & (~intr_mask), intr_loc);
}

// Waits for any of the interrupts in `mask` to happen. If any already
// happened, exits immediately.
// Assumptions: intr_mask was cleared from dev->interrupts and
// BAR0+HARDDOOM_INTR before the event we're waiting for happens.
int doomcmd_interrupts_await(struct harddoom_dev *dev, u32 intr_mask)
{
	return wait_event_interruptible(
	    dev->dev_queue, atomic_read(&dev->interrupts) & intr_mask);
}

inline void doomcmd_clear_interrupts(struct harddoom_dev *dev, u32 intr_mask)
{
	iowrite32(intr_mask, dev->bar_iomap + HARDDOOM_INTR);
	atomic_andnot(intr_mask, &dev->interrupts);
}

// An upper bound on the FENCE cache size.
#define HARDDOOM_FENCE_CACHE (1 << 12)
#define HARDDOOM_SMALL_FENCE_MASK (HARDDOOM_FENCE_MASK >> 1)

static int doomcmd_wait_fence(struct harddoom_dev *dev, u32 fence_value)
{
	__iomem void *iomap = dev->bar_iomap;
	u32 cur_fence;
	int err;

	BUG_ON(fence_value & ~HARDDOOM_FENCE_MASK);
	iowrite32(fence_value, iomap + HARDDOOM_FENCE_WAIT);
	doomcmd_clear_interrupts(dev, HARDDOOM_INTR_FENCE);
	// Check if we haven't reached fence_value yet. It can happen in two
	// situations: if cur_fence hasn't overflown yet, then it's simply
	// larger than fence_value. In the other case, fence_value is
	// "very large" and cur_fence is "very small".
	cur_fence = ioread32(iomap + HARDDOOM_FENCE_LAST);
	if (cur_fence >= fence_value)
		return 0;
	if (cur_fence < HARDDOOM_FENCE_CACHE &&
	    fence_value >= HARDDOOM_SMALL_FENCE_MASK - HARDDOOM_FENCE_CACHE)
		return 0;
	if (unlikely(err = doomcmd_interrupts_await(dev, HARDDOOM_INTR_FENCE)))
		return err;
	iowrite32(HARDDOOM_FENCE_NO_VALUE, iomap + HARDDOOM_FENCE_WAIT);
	return 0;
}

int doomcmd_synchronize(struct harddoom_dev *dev)
{
	int err;
	uint32_t next_fence =
	    (dev->writer.last_fence + 1) & HARDDOOM_SMALL_FENCE_MASK;
	if (dev->writer.is_synchronized)
		return 0;
	if (unlikely(
		err = doomcmd_put_command(dev, HARDDOOM_CMD_FENCE(next_fence))))
		return err;
	dev->writer.last_fence = next_fence;
	if (unlikely(err = doomcmd_wait_fence(dev, next_fence)))
		return err;
	dev->writer.is_synchronized = true;
	release_marked_buffers(dev);
	return 0;
}

int doomcmd_interlock(struct harddoom_dev *dev)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_INTERLOCK);
}

#define DMA_ADDR_ALIGN 0x3f

static int doomcmd_surf_pt(struct harddoom_dev *dev, dma_addr_t ptr,
			   dma_addr_t *prev_ptr, uint32_t cmd)
{
	int err;
	if (unlikely(ptr & DMA_ADDR_ALIGN)) {
		printk(KERN_WARNING "Non-aligned DMA pointer: %llx", (u64)ptr);
		return -EINVAL;
	}
	// Do not update if unnecessary.
	if (ptr == *prev_ptr)
		return 0;

	if (unlikely(err = doomcmd_put_command(dev, cmd)))
		return err;
	*prev_ptr = ptr;
	return 0;
}

int doomcmd_surf_dst_pt(struct harddoom_dev *dev, dma_addr_t ptr)
{
	return doomcmd_surf_pt(dev, ptr, &dev->writer.cur_surf_dst,
			       HARDDOOM_CMD_SURF_DST_PT(ptr));
}

int doomcmd_surf_src_pt(struct harddoom_dev *dev, dma_addr_t ptr)
{
	return doomcmd_surf_pt(dev, ptr, &dev->writer.cur_surf_src,
			       HARDDOOM_CMD_SURF_SRC_PT(ptr));
}

int doomcmd_texture_pt(struct harddoom_dev *dev, dma_addr_t ptr)
{
	return doomcmd_surf_pt(dev, ptr, &dev->writer.cur_texture,
			       HARDDOOM_CMD_TEXTURE_PT(ptr));
}

static int doomcmd_set_addr(struct harddoom_dev *dev, dma_addr_t ptr,
			    dma_addr_t *prev_ptr, u32 align_check, uint32_t cmd)
{
	int err;
	if (unlikely(ptr & align_check)) {
		printk(KERN_WARNING "Non-aligned pointer: %llx", (u64)ptr);
		return -EINVAL;
	}
	// Avoid updates if unnecessary.
	if (ptr == *prev_ptr)
		return 0;
	if (unlikely(err = doomcmd_put_command(dev, cmd)))
		return err;
	*prev_ptr = ptr;
	return 0;
}

int doomcmd_flat_addr(struct harddoom_dev *dev, dma_addr_t ptr)
{
#define FLAT_ADDR_ALIGN 0xfff
	return doomcmd_set_addr(dev, ptr, &dev->writer.cur_flat,
				FLAT_ADDR_ALIGN, HARDDOOM_CMD_FLAT_ADDR(ptr));
}

#define COLORMAP_ALIGN 0xff

int doomcmd_colormap_addr(struct harddoom_dev *dev, dma_addr_t ptr)
{
	return doomcmd_set_addr(dev, ptr, &dev->writer.cur_colormap,
				COLORMAP_ALIGN,
				HARDDOOM_CMD_COLORMAP_ADDR(ptr));
}

int doomcmd_translation_addr(struct harddoom_dev *dev, dma_addr_t ptr)
{
	return doomcmd_set_addr(dev, ptr, &dev->writer.cur_translation,
				COLORMAP_ALIGN,
				HARDDOOM_CMD_TRANSLATION_ADDR(ptr));
}

static int doomcmd_set_dims(struct harddoom_dev *dev, u32 cmd, u32 *last_cmd)
{
	int err;
	// Do not update if unnecessary.
	if (cmd == *last_cmd)
		return 0;
	if (unlikely(err = doomcmd_put_command(dev, cmd)))
		return err;
	*last_cmd = cmd;
	return 0;
}

int doomcmd_texture_dims(struct harddoom_dev *dev, u16 height, u16 sz)
{
	return doomcmd_set_dims(dev, HARDDOOM_CMD_TEXTURE_DIMS(sz, height),
				&dev->writer.last_texture_dims);
}

int doomcmd_surf_dims(struct harddoom_dev *dev, u16 width, u16 height)
{
	return doomcmd_set_dims(dev, HARDDOOM_CMD_SURF_DIMS(width, height),
				&dev->writer.last_surf_dims);
}

int doomcmd_fill_color(struct harddoom_dev *dev, u8 color)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_FILL_COLOR(color));
}

int doomcmd_draw_params(struct harddoom_dev *dev, u8 flags)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_DRAW_PARAMS(flags));
}

int doomcmd_xy_a(struct harddoom_dev *dev, u16 x, u16 y)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_XY_A(x, y));
}

int doomcmd_xy_b(struct harddoom_dev *dev, u16 x, u16 y)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_XY_B(x, y));
}

int doomcmd_ustart(struct harddoom_dev *dev, u32 coord)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_USTART(coord));
}

int doomcmd_vstart(struct harddoom_dev *dev, u32 coord)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_VSTART(coord));
}

int doomcmd_ustep(struct harddoom_dev *dev, u32 delta)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_USTEP(delta));
}

int doomcmd_vstep(struct harddoom_dev *dev, u32 delta)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_VSTEP(delta));
}

int doomcmd_copy_rect(struct harddoom_dev *dev, u16 width, u16 height)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_COPY_RECT(width, height));
}

int doomcmd_fill_rect(struct harddoom_dev *dev, u16 width, u16 height)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_FILL_RECT(width, height));
}

int doomcmd_draw_line(struct harddoom_dev *dev)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_DRAW_LINE);
}

int doomcmd_draw_background(struct harddoom_dev *dev)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_DRAW_BACKGROUND);
}

int doomcmd_draw_column(struct harddoom_dev *dev, u32 offset)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_DRAW_COLUMN(offset));
}

int doomcmd_draw_span(struct harddoom_dev *dev)
{
	return doomcmd_put_command(dev, HARDDOOM_CMD_DRAW_SPAN);
}