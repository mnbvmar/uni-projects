#ifndef SURFACE_H
#define SURFACE_H

#include "buffers.h"

int surface_copy_rects(struct doomdev_surface *surf_dst,
		       struct doomdev_surf_ioctl_copy_rects *op);

int surface_draw_lines(struct doomdev_surface *surf,
		       struct doomdev_surf_ioctl_draw_lines *op);

int surface_fill_rects(struct doomdev_surface *surf,
		       struct doomdev_surf_ioctl_fill_rects *op);

int surface_draw_background(struct doomdev_surface *surf,
			    struct doomdev_surf_ioctl_draw_background *op);

int surface_draw_columns(struct doomdev_surface *surf,
			 struct doomdev_surf_ioctl_draw_columns *op);

int surface_draw_spans(struct doomdev_surface *surf,
		       struct doomdev_surf_ioctl_draw_spans *op);

#endif