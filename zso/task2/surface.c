#include <asm/uaccess.h>
#include <linux/file.h>
#include <linux/list.h>
#include <linux/wait.h>

#include "command.h"
#include "common.h"
#include "doomdev.h"
#include "harddoom.h"
#include "surface.h"

// -- UTILS -- //

inline struct harddoom_dev *fetch_dev(struct doomdev_surface *surf)
{
	return surf->header.idata->hddev;
}

struct operation_init_args {
	struct doomdev_surface *dest_surf; // [in]
	size_t user_data_size;		   // [in]
	void __user *user_ptr;		   // [in]
	void *user_data;		   // [out]
	struct harddoom_dev *dev;	  // [out]
};

static int surface_init_operation(struct operation_init_args *args)
{
	struct dma_pages_info *pinfo = args->dest_surf->header.pages_info;
	dma_addr_t surf_pt = pinfo->page_table_ptr;
	int err;
	args->dev = fetch_dev(args->dest_surf);

	args->user_data = NULL;
	if (args->user_data_size) {
		args->user_data =
		    alloc_from_user(args->user_ptr, args->user_data_size);
		if (IS_ERR(args->user_data))
			return PTR_ERR(args->user_data);
	}

	if (unlikely(err = mutex_lock_interruptible(&args->dev->dev_mutex)))
		goto err_init_mutex;

	if (unlikely(err = doomcmd_surf_dst_pt(args->dev, surf_pt)))
		goto err_init_set_surf;
	if (unlikely(err = doomcmd_surf_dims(
			 args->dev, args->dest_surf->buffer_info.width,
			 args->dest_surf->buffer_info.height)))
		goto err_set_dims;

	return 0;

err_set_dims:
err_init_set_surf:
	mutex_unlock(&args->dev->dev_mutex);
err_init_mutex:
	kfree(args->user_data);
	return err;
}

static void surface_end_operation(struct operation_init_args *args)
{
	mutex_unlock(&args->dev->dev_mutex);
	kfree(args->user_data);
}

static int fd_to_file_typed(int fd, struct file_operations *fop_chk,
			    struct fd *result)
{
	struct file *f;

	*result = fdget(fd);
	f = result->file;
	if (unlikely(!f))
		return -EINVAL;
	if (unlikely(f->f_op != fop_chk)) {
		fdput(*result);
		return -EINVAL;
	}
	return 0;
}

static int perform_interlock(struct harddoom_dev *dev)
{
	struct list_head *elem;
	struct list_head *tmp;
	int err;
	if (unlikely(err = doomcmd_interlock(dev)))
		return err;

	list_for_each_safe(elem, tmp, &dev->writer.unsafe_surfaces)
	{
		struct doomdev_surface *surf = container_of(
		    elem, struct doomdev_surface, copy_unsafe_elem);
		surf->is_copy_safe = true;
		list_del(elem);
	}
	return 0;
}

// -- END UTILS -- //

// -- COPY_RECTS -- //

inline int check_copy_rect(struct doomdev_surface *surf_dst,
			   struct doomdev_surface *surf_src,
			   struct doomdev_copy_rect *rect)
{
	struct doomdev_ioctl_create_surface *dst = &surf_dst->buffer_info;
	struct doomdev_ioctl_create_surface *src = &surf_src->buffer_info;

	if (unlikely((size_t)rect->pos_src_x + rect->width > src->width))
		return -EINVAL;
	if (unlikely((size_t)rect->pos_src_y + rect->height > src->height))
		return -EINVAL;
	if (unlikely((size_t)rect->pos_dst_x + rect->width > dst->width))
		return -EINVAL;
	if (unlikely((size_t)rect->pos_dst_y + rect->height > dst->height))
		return -EINVAL;
	return 0;
}

int surface_copy_rects(struct doomdev_surface *surf_dst,
		       struct doomdev_surf_ioctl_copy_rects *op)
{
	struct doomdev_ioctl_create_surface *dst = &surf_dst->buffer_info;
	struct doomdev_ioctl_create_surface *src;
	struct doomdev_copy_rect *host_rects;
	struct doomdev_surface *surf_src;
	size_t n = 0;
	struct fd src_fd;
	dma_addr_t surf_src_pt;
	int err;
	struct operation_init_args args = {
	    .dest_surf = surf_dst,
	    .user_ptr = (char __user *)op->rects_ptr,
	    .user_data_size =
		(size_t)op->rects_num * sizeof(struct doomdev_copy_rect),
	};

	if ((err =
		 fd_to_file_typed(op->surf_src_fd, &surface_file_ops, &src_fd)))
		return err;
	surf_src = src_fd.file->private_data;
	src = &surf_src->buffer_info;

	if (unlikely(src->width != dst->width))
		return -EINVAL;
	if (unlikely(src->height != dst->height))
		return -EINVAL;

	if (unlikely(err = surface_init_operation(&args))) {
		goto err_init;
		return err;
	}

	surf_src_pt = surf_src->header.pages_info->page_table_ptr;
	if (unlikely(err = doomcmd_surf_src_pt(args.dev, surf_src_pt)))
		goto err_drawing;

	if (!surf_src->is_copy_safe) {
		if (unlikely(err = perform_interlock(args.dev)))
			goto err_drawing;
		BUG_ON(!surf_src->is_copy_safe);
	}

	host_rects = (struct doomdev_copy_rect *)args.user_data;

	while (n < op->rects_num) {
		// Take n-th line, check its validity, and draw it.
		struct doomdev_copy_rect *rect = &host_rects[n];
		if (unlikely(err = check_copy_rect(surf_dst, surf_src, rect)))
			goto err_drawing;
		if (unlikely(err = doomcmd_xy_a(args.dev, rect->pos_src_x,
						rect->pos_src_y)))
			goto err_drawing;
		if (unlikely(err = doomcmd_xy_b(args.dev, rect->pos_dst_x,
						rect->pos_dst_y)))
			goto err_drawing;
		if (unlikely(err = doomcmd_copy_rect(args.dev, rect->width,
						     rect->height)))
			goto err_drawing;
		++n;
	}

	if (surf_dst->is_copy_safe) {
		surf_dst->is_copy_safe = false;
		list_add(&surf_dst->copy_unsafe_elem,
			 &args.dev->writer.unsafe_surfaces);
	}

	fdput(src_fd);
	surface_end_operation(&args);
	return n;

err_drawing:
	surface_end_operation(&args);
err_init:
	fdput(src_fd);
	return n ?: err;
}

// -- DRAW_LINES -- //

inline int check_line(struct doomdev_surface *surf, struct doomdev_line *line)
{
	struct doomdev_ioctl_create_surface *sinfo = &surf->buffer_info;
	if (unlikely(line->pos_a_x >= sinfo->width))
		return -EINVAL;
	if (unlikely(line->pos_a_y >= sinfo->height))
		return -EINVAL;
	if (unlikely(line->pos_b_x >= sinfo->width))
		return -EINVAL;
	if (unlikely(line->pos_b_y >= sinfo->height))
		return -EINVAL;
	return 0;
}

int surface_draw_lines(struct doomdev_surface *surf,
		       struct doomdev_surf_ioctl_draw_lines *op)
{
	struct doomdev_line *host_lines;
	size_t n = 0;
	int err;
	struct operation_init_args args = {
	    .dest_surf = surf,
	    .user_ptr = (char __user *)op->lines_ptr,
	    .user_data_size =
		(size_t)op->lines_num * sizeof(struct doomdev_line),
	};
	if ((err = surface_init_operation(&args)))
		return err;
	host_lines = (struct doomdev_line *)args.user_data;

	while (n < op->lines_num) {
		// Take n-th line, check its validity, and draw it.
		struct doomdev_line *dline = &host_lines[n];
		if (unlikely(err = check_line(surf, dline)))
			goto err_drawing;
		if (unlikely(err = doomcmd_fill_color(args.dev, dline->color)))
			goto err_drawing;
		if (unlikely(err = doomcmd_xy_a(args.dev, dline->pos_a_x,
						dline->pos_a_y)))
			goto err_drawing;
		if (unlikely(err = doomcmd_xy_b(args.dev, dline->pos_b_x,
						dline->pos_b_y)))
			goto err_drawing;
		if (unlikely(err = doomcmd_draw_line(args.dev)))
			goto err_drawing;
		++n;
	}

	surface_end_operation(&args);
	return n;

err_drawing:
	surface_end_operation(&args);
	return n ?: err;
}

// -- FILL_RECTS -- //

inline int check_fill_rect(struct doomdev_surface *surf,
			   struct doomdev_fill_rect *rect)
{
	// Use correct types to avoid overflows.
	if (unlikely((unsigned)rect->pos_dst_x + rect->width >
		     surf->buffer_info.width))
		return -EINVAL;
	if (unlikely((unsigned)rect->pos_dst_y + rect->height >
		     surf->buffer_info.height))
		return -EINVAL;
	return 0;
}

int surface_fill_rects(struct doomdev_surface *surf,
		       struct doomdev_surf_ioctl_fill_rects *op)
{
	struct doomdev_fill_rect *host_rects;
	size_t n = 0;
	int err;
	struct operation_init_args args = {
	    .dest_surf = surf,
	    .user_ptr = (char __user *)op->rects_ptr,
	    .user_data_size =
		(size_t)op->rects_num * sizeof(struct doomdev_fill_rect),
	};
	if (unlikely(err = surface_init_operation(&args)))
		return err;
	host_rects = (struct doomdev_fill_rect *)args.user_data;

	while (n < op->rects_num) {
		struct doomdev_fill_rect *drect = &host_rects[n];
		if (unlikely(err = check_fill_rect(surf, drect)))
			goto err_drawing;
		if (unlikely(err = doomcmd_fill_color(args.dev, drect->color)))
			goto err_drawing;
		if (unlikely(err = doomcmd_xy_a(args.dev, drect->pos_dst_x,
						drect->pos_dst_y)))
			goto err_drawing;
		if (unlikely(err = doomcmd_fill_rect(args.dev, drect->width,
						     drect->height)))
			goto err_drawing;
		++n;
	}

	surface_end_operation(&args);
	return n;

err_drawing:
	surface_end_operation(&args);
	return n ?: err;
}

// -- DRAW_BACKGROUND -- //

int surface_draw_background(struct doomdev_surface *surf,
			    struct doomdev_surf_ioctl_draw_background *op)
{
	struct fd flat_fd;
	struct operation_init_args args = {.dest_surf = surf};
	struct doomdev_flat *dflat;
	int err;
	if (unlikely(
		err = fd_to_file_typed(op->flat_fd, &flat_file_ops, &flat_fd)))
		return err;
	if (unlikely(err = surface_init_operation(&args)))
		goto err_surf;

	dflat = (struct doomdev_flat *)flat_fd.file->private_data;
	if (unlikely(err = doomcmd_flat_addr(
			 args.dev, dflat->header.pages_info->dev_pages[0])))
		goto err_drawing;
	if (unlikely(err = doomcmd_draw_background(args.dev)))
		goto err_drawing;
	err = 0;

	surface_end_operation(&args);

err_drawing:
err_surf:
	fdput(flat_fd);
	return err;
}

// -- DRAW_SPANS and DRAW_COLUMNS COMMON -- //

struct surface_draw_level_data {
	struct operation_init_args args;
	struct fd level_fd; // texture_fd or flat_fd
	struct fd translation_fd;
	struct fd colormap_fd;
	struct doomdev_texture *level;
	struct doomdev_colormaps *translation;
	struct doomdev_colormaps *colormap;
	struct doomdev_surf_ioctl_draw_columns *op;
	dma_addr_t translate_addr;
};

#define DOOMDEV_DRAW_FLAGS_COLORMAP_FUZZ 0x05
#define DOOMDEV_DRAW_FLAGS_TRANSLATE_COLORMAP 0x06
#define DOOMDEV_DRAW_FLAGS_ALL 0x07

static dma_addr_t get_colormap_ptr(struct doomdev_colormaps *cmap, size_t idx)
{
	BUILD_BUG_ON(HARDDOOM_PAGE_SIZE % HARDDOOM_COLORMAP_SIZE != 0);
#define COLORMAPS_PER_PAGE (HARDDOOM_PAGE_SIZE / HARDDOOM_COLORMAP_SIZE)
	return cmap->header.pages_info->dev_pages[idx / COLORMAPS_PER_PAGE] +
	       (idx % COLORMAPS_PER_PAGE) * HARDDOOM_COLORMAP_SIZE;
}

// HACK: you can also pass 'struct surface_surf_ioctl_draw_spans' as op.
// This and 'struct doomdev_surf_ioctl_draw_columns' have the same structure.
static int init_surface_draw_level_data(
    struct surface_draw_level_data *data, struct doomdev_surface *surf,
    struct doomdev_surf_ioctl_draw_columns *op,
    struct file_operations *level_fd_file_ops, size_t doomdev_level_sz)
{
	int err;
	if (unlikely(err = fd_to_file_typed(op->texture_fd, level_fd_file_ops,
					    &data->level_fd)))
		return err;
	data->level = data->level_fd.file->private_data;

	data->colormap_fd.flags = 0;
	if (op->draw_flags & DOOMDEV_DRAW_FLAGS_COLORMAP_FUZZ) {
		if (unlikely(err = fd_to_file_typed(op->colormaps_fd,
						    &colormaps_file_ops,
						    &data->colormap_fd)))
			goto err_colormap;
		data->colormap = data->colormap_fd.file->private_data;
	}

	data->translation_fd.flags = 0;
	if (op->draw_flags & DOOMDEV_DRAW_FLAGS_TRANSLATE) {
		struct doomdev_colormaps *cmap;
		if (unlikely(err = fd_to_file_typed(op->translations_fd,
						    &colormaps_file_ops,
						    &data->translation_fd)))
			goto err_translation;
		// Check translation_idx before we do heavy duty with device.
		cmap = data->translation_fd.file->private_data;
		if (unlikely(op->translation_idx >= cmap->buffer_info.num)) {
			err = -EINVAL;
			goto err_translation2;
		}
		data->translation = data->translation_fd.file->private_data;
	}

	data->args = (struct operation_init_args){
	    .dest_surf = surf,
	    .user_ptr = (char __user *)op->columns_ptr,
	    .user_data_size = (size_t)op->columns_num * doomdev_level_sz};
	if (unlikely(err = surface_init_operation(&data->args)))
		goto err_surf;

	if (op->draw_flags & DOOMDEV_DRAW_FLAGS_TRANSLATE)
		data->translate_addr =
		    get_colormap_ptr(data->translation, op->translation_idx);

	data->op = op;

	return 0;

err_surf:
err_translation2:
	fdput(data->translation_fd); // no-op if uninitialized
err_translation:
	fdput(data->colormap_fd); // no-op if uninitialized
err_colormap:
	fdput(data->level_fd);
	return err;
}

static void
destroy_surface_draw_level_data(struct surface_draw_level_data *data)
{
	surface_end_operation(&data->args);
	fdput(data->translation_fd);
	fdput(data->colormap_fd);
	fdput(data->level_fd);
}

// -- DRAW_COLUMNS -- //

#define HARDDOOM_MAX_FIXED_NUM 0x3ffffff

static int check_column(struct surface_draw_level_data *data,
			struct doomdev_column *col)
{
	struct doomdev_ioctl_create_surface *surf_info;
	u32 flags = data->op->draw_flags;
	if (unlikely(col->y1 > col->y2))
		return -EINVAL;
	if (!(flags & DOOMDEV_DRAW_FLAGS_FUZZ))
		if (unlikely(col->ustart > HARDDOOM_MAX_FIXED_NUM))
			return -EINVAL;
	if (unlikely(col->ustep > HARDDOOM_MAX_FIXED_NUM))
		return -EINVAL;
	surf_info = &data->args.dest_surf->buffer_info;
	if (unlikely(col->x >= surf_info->width))
		return -EINVAL;
	if (unlikely(col->y2 >= surf_info->height))
		return -EINVAL;
	// We don't need to check if we go outside the texture.
	// In such a case, the driver will draw a pixel of color 0.
	// We only need to check if the offset fits in 22 bits.
	if (unlikely(col->texture_offset >= (1 << 22)))
		return -EINVAL;

	if (flags & DOOMDEV_DRAW_FLAGS_COLORMAP_FUZZ) {
		struct doomdev_colormaps *cmap =
		    data->colormap_fd.file->private_data;
		if (unlikely(col->colormap_idx >= cmap->buffer_info.num))
			return -EINVAL;
	}
	return 0;
}

int surface_draw_columns(struct doomdev_surface *surf,
			 struct doomdev_surf_ioctl_draw_columns *op)
{
	struct surface_draw_level_data data;
	struct doomdev_column *columns;
	struct doomdev_column *col;
	struct harddoom_dev *dev;
	dma_addr_t text_page_table;
	size_t n = 0;
	int err;

	if (unlikely(op->draw_flags & ~DOOMDEV_DRAW_FLAGS_ALL))
		return -EINVAL;
	if (op->draw_flags & DOOMDEV_DRAW_FLAGS_FUZZ)
		if (unlikely(op->draw_flags &
			     DOOMDEV_DRAW_FLAGS_TRANSLATE_COLORMAP))
			return -EINVAL;

	if (unlikely(err = init_surface_draw_level_data(
			 &data, surf, op, &texture_file_ops,
			 sizeof(struct doomdev_column))))
		return err;
	columns = (struct doomdev_column *)data.args.user_data;
	dev = data.args.dev;

	if (op->draw_flags & DOOMDEV_DRAW_FLAGS_TRANSLATE)
		if (unlikely(err = doomcmd_translation_addr(
				 dev, data.translate_addr)))
			goto err_drawing;

	text_page_table = data.level->header.pages_info->page_table_ptr;
	if (unlikely(err = doomcmd_texture_pt(dev, text_page_table)))
		goto err_drawing;
	if (unlikely(err = doomcmd_texture_dims(dev,
						data.level->buffer_info.height,
						data.level->buffer_info.size)))
		goto err_drawing;
	if (unlikely(err = doomcmd_draw_params(dev, op->draw_flags)))
		goto err_drawing;

	while (n < op->columns_num) {
		col = &columns[n];
		if (unlikely(err = check_column(&data, col)))
			goto err_drawing;

		if (op->draw_flags & DOOMDEV_DRAW_FLAGS_COLORMAP_FUZZ) {
			dma_addr_t addr =
			    get_colormap_ptr(data.colormap, col->colormap_idx);
			if (unlikely(err = doomcmd_colormap_addr(dev, addr)))
				goto err_drawing;
		}

		if (!(op->draw_flags & DOOMDEV_DRAW_FLAGS_FUZZ))
			if (unlikely(err = doomcmd_ustart(dev, col->ustart)))
				goto err_drawing;
		if (unlikely(err = doomcmd_ustep(dev, col->ustep)))
			goto err_drawing;
		if (unlikely(err = doomcmd_xy_a(dev, col->x, col->y1)))
			goto err_drawing;
		if (unlikely(err = doomcmd_xy_b(dev, col->x, col->y2)))
			goto err_drawing;
		if (unlikely(err =
				 doomcmd_draw_column(dev, col->texture_offset)))
			goto err_drawing;

		++n;
	}

	destroy_surface_draw_level_data(&data);
	return n;

err_drawing:
	destroy_surface_draw_level_data(&data);
	return n ?: err;
}

// -- DRAW_SPANS -- //

static int check_span(struct surface_draw_level_data *data,
		      struct doomdev_span *span)
{
	struct doomdev_ioctl_create_surface *surf_info;
	if (unlikely(span->x1 > span->x2))
		return -EINVAL;
	span->ustart &= HARDDOOM_MAX_FIXED_NUM;
	span->vstart &= HARDDOOM_MAX_FIXED_NUM;
	span->ustep &= HARDDOOM_MAX_FIXED_NUM;
	span->ustep &= HARDDOOM_MAX_FIXED_NUM;
	surf_info = &data->args.dest_surf->buffer_info;
	if (unlikely(span->x2 >= surf_info->width))
		return -EINVAL;
	if (unlikely(span->y >= surf_info->height))
		return -EINVAL;
	return 0;
}

int surface_draw_spans(struct doomdev_surface *surf,
		       struct doomdev_surf_ioctl_draw_spans *op)
{
	struct surface_draw_level_data data;
	struct doomdev_span *spans;
	struct harddoom_dev *dev;
	dma_addr_t flat_ptr;
	size_t n = 0;
	int err;

	if (unlikely(op->draw_flags & ~DOOMDEV_DRAW_FLAGS_TRANSLATE_COLORMAP))
		return -EINVAL;
	if (unlikely(err = init_surface_draw_level_data(
			 &data, surf,
			 (struct doomdev_surf_ioctl_draw_columns *)op,
			 &flat_file_ops, sizeof(struct doomdev_span))))
		return err;
	spans = (struct doomdev_span *)data.args.user_data;
	dev = data.args.dev;

	if (op->draw_flags & DOOMDEV_DRAW_FLAGS_TRANSLATE)
		if (unlikely(err = doomcmd_translation_addr(
				 dev, data.translate_addr)))
			goto err_drawing;

	flat_ptr = data.level->header.pages_info->dev_pages[0];
	if (unlikely(err = doomcmd_flat_addr(dev, flat_ptr)))
		goto err_drawing;
	if (unlikely(err = doomcmd_draw_params(dev, op->draw_flags)))
		goto err_drawing;

	while (n < op->spans_num) {
		struct doomdev_span *span = &spans[n];
		if (unlikely(err = check_span(&data, span)))
			goto err_drawing;

		if (unlikely(err = doomcmd_ustart(dev, span->ustart)))
			goto err_drawing;
		if (unlikely(err = doomcmd_vstart(dev, span->vstart)))
			goto err_drawing;
		if (unlikely(err = doomcmd_ustep(dev, span->ustep)))
			goto err_drawing;
		if (unlikely(err = doomcmd_vstep(dev, span->ustep)))
			goto err_drawing;
		if (unlikely(err = doomcmd_xy_a(dev, span->x1, span->y)))
			goto err_drawing;
		if (unlikely(err = doomcmd_xy_b(dev, span->x2, span->y)))
			goto err_drawing;
		if (unlikely(err = doomcmd_draw_span(dev)))
			goto err_drawing;

		++n;
	}

	destroy_surface_draw_level_data(&data);
	return n;

err_drawing:
	destroy_surface_draw_level_data(&data);
	return n ?: err;
}