#ifndef BUFFERS_H
#define BUFFERS_H

#include <linux/fs.h>
#include <linux/list.h>

#include "common.h"
#include "doomdev.h"

struct harddoom_inode_data;

struct doomdev_buffer_header {
	struct list_head elem_buffers;
	struct harddoom_inode_data *idata;
	struct dma_pages_info *pages_info;
	struct file *buf_file;
	int fd;
	bool is_surface;
	size_t buf_size;
};

struct doomdev_surface {
	struct doomdev_buffer_header header; // Must be first.
	struct doomdev_ioctl_create_surface buffer_info;
	// Is it safe to copy from this surface?
	bool is_copy_safe;
	// If unsafe, this is an element of dev.writer.unsafe_surfaces list
	// (so that INTERLOCK operation can mark the surface safe).
	struct list_head copy_unsafe_elem;
};

extern struct file_operations surface_file_ops;

struct doomdev_texture {
	struct doomdev_buffer_header header; // Must be first.
	struct doomdev_ioctl_create_texture buffer_info;
};

extern struct file_operations texture_file_ops;

struct doomdev_flat {
	struct doomdev_buffer_header header; // Must be first.
	struct doomdev_ioctl_create_flat buffer_info;
};

extern struct file_operations flat_file_ops;

struct doomdev_colormaps {
	struct doomdev_buffer_header header; // Must be first.
	struct doomdev_ioctl_create_colormaps buffer_info;
};

extern struct file_operations colormaps_file_ops;

void *alloc_from_user(char __user *buf, size_t nbytes);

long harddoom_create_surface(struct harddoom_inode_data *inode_data,
			     struct doomdev_ioctl_create_surface *args);

long harddoom_create_texture(struct harddoom_inode_data *inode_data,
			     struct doomdev_ioctl_create_texture *args);

long harddoom_create_flat(struct harddoom_inode_data *inode_data,
			  struct doomdev_ioctl_create_flat *args);

long harddoom_create_colormaps(struct harddoom_inode_data *inode_data,
			       struct doomdev_ioctl_create_colormaps *args);

// Release the buffer (in userspace) and mark it ready to be freed
// by kernel.
void harddoom_release_buffer(struct doomdev_buffer_header *bufh);

// Free the memory used by buffer. Unfortunately, it cannot be done
// immediately when we free it (together with `release`).
void harddoom_free_buffer(struct doomdev_buffer_header *bufh);

struct dma_pages_info *harddoom_alloc_cmd_range(struct harddoom_dev *dev,
						long num_pages);

void harddoom_free_cmd_range(struct dma_pages_info *info);

#endif