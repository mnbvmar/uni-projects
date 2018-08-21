#include <asm/string.h>
#include <asm/uaccess.h>
#include <linux/anon_inodes.h>
#include <linux/dmapool.h>
#include <linux/file.h>
#include <linux/fs.h>
#include <linux/module.h>
#include <linux/slab.h>

#include "buffers.h"
#include "command.h"
#include "common.h"
#include "harddoom.h"
#include "surface.h"

// -- HELPERS -- //

void *alloc_from_user(char __user *buf, size_t nbytes)
{
	char *host_data = kmalloc(nbytes, GFP_KERNEL);
	if (unlikely(!host_data))
		return ERR_PTR(-ENOMEM);
	if (unlikely(copy_from_user(host_data, buf, nbytes))) {
		kfree(host_data);
		return ERR_PTR(-EFAULT);
	}
	return host_data;
}

// -- DMA FUNCTIONS -- //

static struct dma_pages_info *dma_pages_info_alloc(size_t num_pages)
{
	struct dma_pages_info *dma_info;

	dma_info = kzalloc(sizeof(struct dma_pages_info), GFP_KERNEL);
	if (IS_ERR_OR_NULL(dma_info))
		goto err_dma_info;

	dma_info->num_pages = num_pages;
	dma_info->host_pages = kmalloc(sizeof(void *) * num_pages, GFP_KERNEL);
	if (IS_ERR_OR_NULL(dma_info->host_pages))
		goto err_host_pages;
	dma_info->dev_pages =
	    kmalloc(sizeof(dma_addr_t) * num_pages, GFP_KERNEL);
	if (IS_ERR_OR_NULL(dma_info->dev_pages))
		goto err_dev_pages;

	return dma_info;

err_dev_pages:
	kfree(dma_info->host_pages);
err_host_pages:
	kfree(dma_info);
err_dma_info:
	return ERR_PTR(-ENOMEM);
}

static void dma_pages_info_free(struct dma_pages_info *dma_info)
{
	kfree(dma_info->dev_pages);
	kfree(dma_info->host_pages);
	kfree(dma_info);
}

static struct dma_pages_info *dma_alloc_pages(struct dma_pool *pool,
					      size_t num_pages)
{
	struct dma_pages_info *pages_info;
	size_t num_alloced = 0;
	int err;

	pages_info = dma_pages_info_alloc(num_pages);
	if (IS_ERR(pages_info))
		return pages_info;
	pages_info->dma_pool = pool;

	while (num_alloced < num_pages) {
		pages_info->host_pages[num_alloced] = dma_pool_alloc(
		    pool, GFP_KERNEL, &pages_info->dev_pages[num_alloced]);
		if (unlikely(!pages_info->host_pages[num_alloced])) {
			err = -ENOMEM;
			goto err_dma_pages;
		}
		BUG_ON(pages_info->dev_pages[num_alloced] % HARDDOOM_PAGE_SIZE);
		++num_alloced;
	}
	return pages_info;

err_dma_pages:
	while (num_alloced > 0) {
		dma_pool_free(pool, pages_info->host_pages[num_alloced - 1],
			      pages_info->dev_pages[num_alloced - 1]);
		--num_alloced;
	}
	dma_pages_info_free(pages_info);
	return ERR_PTR(err);
}

static void dma_discard_pages(struct dma_pages_info *pages_info)
{
	size_t idx = 0;
	for (idx = 0; idx < pages_info->num_pages; ++idx)
		dma_pool_free(pages_info->dma_pool, pages_info->host_pages[idx],
			      pages_info->dev_pages[idx]);
	dma_pages_info_free(pages_info);
}

static void dma_pages_save_page_table(struct dma_pages_info *dma_info,
				      size_t offset_page_table,
				      size_t num_pages_tabled)
{
	void *last_page = dma_info->host_pages[dma_info->num_pages - 1];
	uint32_t *page_table = (uint32_t *)(last_page + offset_page_table);
	dma_addr_t dev_last_page = dma_info->dev_pages[dma_info->num_pages - 1];
	uint32_t idx;

	for (idx = 0; idx < num_pages_tabled; ++idx) {
		page_table[idx] =
		    (dma_info->dev_pages[idx] & HARDDOOM_TLB_ENTRY_PAGE_MASK) |
		    HARDDOOM_TLB_ENTRY_PTE_VALID;
	}

	dma_info->page_table_ptr = dev_last_page + offset_page_table;
}

static struct dma_pages_info *alloc_dma_paged(struct dma_pool *pool,
					      size_t req_size)
{
	size_t num_user_pages = DIV_ROUND_UP(req_size, HARDDOOM_PAGE_SIZE);
	size_t page_table_size =
	    roundup(num_user_pages * sizeof(uint32_t), HARDDOOM_PAGE_ALIGNMENT);
	size_t last_page_space;
	size_t num_total_pages;
	size_t offset_page_table;
	struct dma_pages_info *pages_info;

	if (page_table_size > HARDDOOM_PAGE_SIZE)
		return ERR_PTR(-EINVAL);

	last_page_space = num_user_pages * HARDDOOM_PAGE_SIZE - req_size;
	if (last_page_space >= page_table_size) {
		num_total_pages = num_user_pages;
		offset_page_table = HARDDOOM_PAGE_SIZE - page_table_size;
	} else {
		num_total_pages = num_user_pages + 1;
		offset_page_table = 0;
	}

	pages_info = dma_alloc_pages(pool, num_total_pages);
	if (IS_ERR(pages_info))
		return pages_info;
	dma_pages_save_page_table(pages_info, offset_page_table,
				  num_user_pages);
	return pages_info;
}

static struct dma_pages_info *alloc_dma_nonpaged(struct dma_pool *pool,
						 size_t req_size)
{
	size_t num_user_pages = DIV_ROUND_UP(req_size, HARDDOOM_PAGE_SIZE);
	return dma_alloc_pages(pool, num_user_pages);
}

static void copy_to_dma(struct dma_pages_info *pages_info, size_t pages_size,
			char *data, size_t data_size)
{
	size_t page_idx = 0;
	const size_t page_size = HARDDOOM_PAGE_SIZE;

	BUG_ON(data_size > pages_size);
	BUG_ON(pages_size > pages_info->num_pages * HARDDOOM_PAGE_SIZE);

	while (pages_size > 0) {
		size_t copy_bytes = min(data_size, page_size);
		size_t has_bytes = min(pages_size, page_size);
		// Copy bytes from data. If we run out of it, fill the
		// remaining space with zeros.
		if (copy_bytes)
			memcpy(pages_info->host_pages[page_idx], data,
			       copy_bytes);
		if (has_bytes > copy_bytes)
			memset(pages_info->host_pages[page_idx] + copy_bytes, 0,
			       has_bytes - copy_bytes);
		++page_idx;
		pages_size -= has_bytes;
		data_size -= copy_bytes;
		data += copy_bytes;
	}
}

// -- SURFACE FILE FUNCTIONS -- //

static loff_t surface_llseek(struct file *filep, loff_t offset, int whence)
{
	loff_t new_loc;
	struct doomdev_surface *surf = filep->private_data;
	switch (whence) {
	case SEEK_SET:
		new_loc = offset;
		break;
	case SEEK_CUR:
		new_loc = filep->f_pos + offset;
		break;
	case SEEK_END:
		new_loc = surf->header.buf_size - offset;
		break;
	default:
		return -EINVAL;
	}
	if (unlikely(new_loc < 0 || new_loc >= surf->header.buf_size))
		return -EINVAL;
	filep->f_pos = new_loc;
	return new_loc;
}

static ssize_t surface_single_page_read(struct dma_pages_info *pages_info,
					char __user *buf, size_t count,
					loff_t cur_offset)
{
	size_t page_id = cur_offset / HARDDOOM_PAGE_SIZE;
	size_t page_loc = cur_offset % HARDDOOM_PAGE_SIZE;
	size_t bytes_remaining =
	    (page_id + 1) * HARDDOOM_PAGE_SIZE - cur_offset;
	size_t n = min(count, bytes_remaining);
	if (unlikely(copy_to_user(
		buf, pages_info->host_pages[page_id] + page_loc, n)))
		return -EFAULT;
	return n;
}

static ssize_t surface_read(struct file *filep, char __user *buf, size_t count,
			    loff_t *cur_offset)
{
	struct doomdev_surface *surf = filep->private_data;
	struct doomdev_buffer_header *bufh = &surf->header;
	ssize_t num_written = 0;
	ssize_t res_write;
	size_t max_count;

	// Synchronize with device.
	doomcmd_synchronize(surf->header.idata->hddev);

	// Check the bounds. Beware of overflows.
	if (unlikely(*cur_offset >= bufh->buf_size))
		return -EINVAL;
	max_count = bufh->buf_size - *cur_offset;
	count = min(count, max_count);

	while (count) {
		res_write = surface_single_page_read(bufh->pages_info, buf,
						     count, *cur_offset);
		if (unlikely(res_write <= 0))
			return num_written ?: res_write;
		num_written += res_write;
		count -= res_write;
		buf += res_write;
		*cur_offset += res_write;
	}
	return num_written;
}

static long surface_ioctl(struct file *filep, unsigned int cmd,
			  unsigned long user_arg)
{
	struct doomdev_surface *surf = filep->private_data;
	void *arg;
	int err;

	switch (cmd) {
	case DOOMDEV_SURF_IOCTL_COPY_RECTS:
		arg = alloc_from_user(
		    (char __user *)user_arg,
		    sizeof(struct doomdev_surf_ioctl_copy_rects));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = surface_copy_rects(filep->private_data, arg);
		kfree(arg);
		break;

	case DOOMDEV_SURF_IOCTL_FILL_RECTS:
		arg = alloc_from_user(
		    (char __user *)user_arg,
		    sizeof(struct doomdev_surf_ioctl_fill_rects));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = surface_fill_rects(filep->private_data, arg);
		kfree(arg);
		break;

	case DOOMDEV_SURF_IOCTL_DRAW_LINES:
		arg = alloc_from_user(
		    (char __user *)user_arg,
		    sizeof(struct doomdev_surf_ioctl_draw_lines));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = surface_draw_lines(surf, arg);
		kfree(arg);
		break;

	case DOOMDEV_SURF_IOCTL_DRAW_BACKGROUND:
		arg = alloc_from_user(
		    (char __user *)user_arg,
		    sizeof(struct doomdev_surf_ioctl_draw_background));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = surface_draw_background(surf, arg);
		kfree(arg);
		break;

	case DOOMDEV_SURF_IOCTL_DRAW_COLUMNS:
		arg = alloc_from_user(
		    (char __user *)user_arg,
		    sizeof(struct doomdev_surf_ioctl_draw_columns));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = surface_draw_columns(surf, arg);
		kfree(arg);
		break;

	case DOOMDEV_SURF_IOCTL_DRAW_SPANS:
		arg = alloc_from_user(
		    (char __user *)user_arg,
		    sizeof(struct doomdev_surf_ioctl_draw_spans));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = surface_draw_spans(surf, arg);
		kfree(arg);
		break;

	default:
		err = -ENOTTY;
		break;
	}

	return err;
}

// -- ALL BUFFER TYPES HELPERS -- //

struct file_operations surface_file_ops = {
    .owner = THIS_MODULE,
    .unlocked_ioctl = surface_ioctl,
    .compat_ioctl = surface_ioctl,
    .llseek = surface_llseek,
    .read = surface_read,
};

static void *harddoom_create_buffer(struct harddoom_inode_data *inode_data,
				    size_t descr_size, const char *type_name,
				    struct file_operations *descr_file_ops)
{
	struct doomdev_buffer_header *header = kzalloc(descr_size, GFP_KERNEL);
	int err;
	struct file *buffer_file;

	if (unlikely(!header))
		return ERR_PTR(-ENOMEM);

	buffer_file = anon_inode_getfile(type_name, descr_file_ops,
					 (void *)header, O_RDONLY);
	if (IS_ERR(buffer_file)) {
		err = PTR_ERR(buffer_file);
		goto err_sfile;
	}
	buffer_file->f_mode |= FMODE_LSEEK | FMODE_PREAD | FMODE_READ;

	header->idata = inode_data;
	header->buf_file = buffer_file;
	header->fd = get_unused_fd_flags(0);
	if (IS_ERR_VALUE((long)header->fd)) {
		err = header->fd;
		put_unused_fd(header->fd);
		goto err_unused_fd;
	}
	fd_install(header->fd, buffer_file);

	if (unlikely(err = mutex_lock_interruptible(&inode_data->inode_mutex)))
		goto err_mutex;
	list_add(&header->elem_buffers, &inode_data->list_buffers);
	mutex_unlock(&inode_data->inode_mutex);
	return header;

err_mutex:
err_unused_fd:
	fput(buffer_file);
err_sfile:
	kfree(header);
	return ERR_PTR(err);
}

// -- SURFACE -- //

#define HARDDOOM_MAX_SURFACE_DIM 2048

long harddoom_create_surface(struct harddoom_inode_data *inode_data,
			     struct doomdev_ioctl_create_surface *args)
{
	struct dma_pages_info *pages_info;
	int err;
	struct doomdev_surface *surface;
	size_t buf_size;

	if (unlikely(args->width == 0 ||
		     args->width > HARDDOOM_MAX_SURFACE_DIM))
		return -EINVAL;
	if (unlikely(args->width % 64 != 0))
		return -EINVAL;
	if (unlikely(args->height == 0 ||
		     args->height > HARDDOOM_MAX_SURFACE_DIM))
		return -EINVAL;

	buf_size = args->width * args->height;
	pages_info = alloc_dma_paged(inode_data->hddev->dev_dma_pool, buf_size);
	if (IS_ERR(pages_info))
		return PTR_ERR(pages_info);

	surface =
	    harddoom_create_buffer(inode_data, sizeof(struct doomdev_surface),
				   "hdsurface", &surface_file_ops);
	if (IS_ERR(surface)) {
		err = PTR_ERR(surface);
		goto err_surface;
	}

	surface->header.pages_info = pages_info;
	surface->buffer_info = *args;
	surface->is_copy_safe = true;
	surface->header.buf_size = buf_size;
	surface->header.is_surface = true;
	INIT_LIST_HEAD(&surface->copy_unsafe_elem);
	return surface->header.fd;

err_surface:
	dma_discard_pages(pages_info);
	return err;
}

// -- TEXTURE -- //

#define HARDDOOM_MAX_TEXTURE_SIZE (4 * 1024 * 1024)
#define HARDDOOM_TEXTURE_PAD 256

// Don't merge with other similar file_operations structs. We use different
// file_operations structs to check if provided file descriptors are
// of the correct type.
struct file_operations texture_file_ops = {
    .owner = THIS_MODULE,
};

long harddoom_create_texture(struct harddoom_inode_data *inode_data,
			     struct doomdev_ioctl_create_texture *args)
{
	struct dma_pages_info *pages_info;
	struct doomdev_texture *texture;
	char *host_texture;
	size_t buf_size;
	int err;

	if (unlikely(args->size > HARDDOOM_MAX_TEXTURE_SIZE))
		return -EINVAL;
	buf_size = roundup(args->size, HARDDOOM_TEXTURE_PAD);

	host_texture =
	    alloc_from_user((void __user *)args->data_ptr, args->size);
	if (IS_ERR(host_texture))
		return PTR_ERR(host_texture);

	pages_info = alloc_dma_paged(inode_data->hddev->dev_dma_pool, buf_size);
	if (IS_ERR(pages_info)) {
		err = PTR_ERR(pages_info);
		goto err_dma_alloc;
	}

	copy_to_dma(pages_info, buf_size, host_texture, args->size);

	texture =
	    harddoom_create_buffer(inode_data, sizeof(struct doomdev_texture),
				   "hdtexture", &texture_file_ops);
	if (IS_ERR(texture)) {
		err = PTR_ERR(texture);
		goto err_texture;
	}

	texture->header.pages_info = pages_info;
	texture->buffer_info = *args;
	texture->header.buf_size = buf_size;
	return texture->header.fd;

err_texture:
	dma_discard_pages(pages_info);
err_dma_alloc:
	kfree(host_texture);
	return err;
}

// -- FLAT -- //

struct file_operations flat_file_ops = {
    .owner = THIS_MODULE,
};

long harddoom_create_flat(struct harddoom_inode_data *inode_data,
			  struct doomdev_ioctl_create_flat *args)
{
	struct dma_pages_info *pages_info;
	struct doomdev_flat *flat;
	char *host_flat;
	int err;

	BUILD_BUG_ON(HARDDOOM_FLAT_SIZE > HARDDOOM_PAGE_SIZE);

	host_flat =
	    alloc_from_user((void __user *)args->data_ptr, HARDDOOM_FLAT_SIZE);
	if (IS_ERR(host_flat))
		return PTR_ERR(host_flat);

	pages_info = alloc_dma_nonpaged(inode_data->hddev->dev_dma_pool,
					HARDDOOM_FLAT_SIZE);
	if (IS_ERR(pages_info)) {
		err = PTR_ERR(pages_info);
		goto err_dma_alloc;
	}

	copy_to_dma(pages_info, HARDDOOM_FLAT_SIZE, host_flat,
		    HARDDOOM_FLAT_SIZE);

	flat = harddoom_create_buffer(inode_data, sizeof(struct doomdev_flat),
				      "hdflat", &flat_file_ops);
	if (IS_ERR(flat)) {
		err = PTR_ERR(flat);
		goto err_flat;
	}

	flat->header.pages_info = pages_info;
	flat->buffer_info = *args;
	flat->header.buf_size = HARDDOOM_FLAT_SIZE;
	return flat->header.fd;

err_flat:
	dma_discard_pages(pages_info);
err_dma_alloc:
	kfree(host_flat);
	return err;
}

// -- COLORMAPS -- //

struct file_operations colormaps_file_ops = {
    .owner = THIS_MODULE,
};

long harddoom_create_colormaps(struct harddoom_inode_data *inode_data,
			       struct doomdev_ioctl_create_colormaps *args)
{
	struct dma_pages_info *pages_info;
	struct doomdev_colormaps *colormaps;
	char *host_colormaps;
	size_t alloc_size;
	int err;

	alloc_size = args->num * HARDDOOM_COLORMAP_SIZE;
	host_colormaps =
	    alloc_from_user((void __user *)args->data_ptr, alloc_size);
	if (IS_ERR(host_colormaps))
		return PTR_ERR(host_colormaps);

	pages_info =
	    alloc_dma_nonpaged(inode_data->hddev->dev_dma_pool, alloc_size);
	if (IS_ERR(pages_info)) {
		err = PTR_ERR(pages_info);
		goto err_dma_alloc;
	}

	copy_to_dma(pages_info, alloc_size, host_colormaps, alloc_size);

	colormaps =
	    harddoom_create_buffer(inode_data, sizeof(struct doomdev_colormaps),
				   "hdcolors", &colormaps_file_ops);
	if (IS_ERR(colormaps)) {
		err = PTR_ERR(colormaps);
		goto err_colormaps;
	}

	colormaps->header.pages_info = pages_info;
	colormaps->buffer_info = *args;
	colormaps->header.buf_size = alloc_size;
	return colormaps->header.fd;

err_colormaps:
	dma_discard_pages(pages_info);
err_dma_alloc:
	kfree(host_colormaps);
	return err;
}

// -- CLEANUP -- //

// Assumes that bufh->elem_buffers is a list of inode->list_buffers,
// dev mutex is taken, and the device is synchronized or off.
void harddoom_release_buffer(struct doomdev_buffer_header *bufh)
{
	fput(bufh->buf_file);
	if (bufh->is_surface) {
		struct doomdev_surface *surf = (struct doomdev_surface *)bufh;
		if (!surf->is_copy_safe)
			list_del(&surf->copy_unsafe_elem);
	}
}

// Assumes that bufh->elem_buffers is a list of dev->list_buffers_release,
// dev mutex is taken, and the device is synchronized or off.
void harddoom_free_buffer(struct doomdev_buffer_header *bufh)
{
	dma_discard_pages(bufh->pages_info);
	list_del(&bufh->elem_buffers);
	kfree(bufh);
}