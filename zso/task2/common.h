#ifndef COMMON_H
#define COMMON_H

#include <linux/atomic.h>
#include <linux/cdev.h>
#include <linux/interrupt.h>
#include <linux/pci.h>
#include <linux/wait.h>

struct dma_pages_info {
	size_t num_pages;
	struct dma_pool *dma_pool;
	void **host_pages;
	dma_addr_t *dev_pages;
	dma_addr_t page_table_ptr;
};

struct harddoom_dev_writer {
	// Number of commands after last async ping.
	u8 cmds_from_async;
	// Is the device synchronized with the driver? (It happens when we have
	// already waited for it and no operation came since.)
	bool is_synchronized;
	// Current dest, src, texture buffers etc.
	dma_addr_t cur_surf_dst;
	dma_addr_t cur_surf_src;
	dma_addr_t cur_texture;
	dma_addr_t cur_flat;
	dma_addr_t cur_colormap;
	dma_addr_t cur_translation; // TODO: clear these sometimes
	uint32_t last_texture_dims;
	uint32_t last_surf_dims;

	// List of surfaces that can't be a CopyRect source (without INTERLOCK).
	struct list_head unsafe_surfaces;

	// Last inserted FENCE number.
	uint32_t last_fence;
};

#define HARDDOOM_NUM_CMD_PAGES 128
// A value FENCE can never attain.
#define HARDDOOM_FENCE_NO_VALUE HARDDOOM_FENCE_MASK

struct harddoom_dev {
	// Device pointers associated with this device.
	struct pci_dev *pdev;
	struct device *userdev;
	// BAR registers.
	void __iomem *bar_iomap;

	struct cdev cdev;

	// A mask of yet unhandled interrupts.
	atomic_t interrupts;

	struct list_head list_buffers_release;
	struct list_head open_descriptors;
	struct mutex dev_mutex;
	wait_queue_head_t dev_queue;
	spinlock_t irq_spinlock;
	struct tasklet_struct irq_tasklet;

	struct dma_pool *dev_dma_pool;

	struct harddoom_dev_writer writer;
};

// Releases all the buffers marked for deletion. Assumes that dev mutex is taken
// and the device is either synchronized or off.
void release_marked_buffers(struct harddoom_dev *dev);

struct harddoom_inode_data {
	struct list_head list_buffers;
	struct harddoom_dev *hddev;
	struct list_head list_inodes;
	struct mutex inode_mutex;
};

#endif