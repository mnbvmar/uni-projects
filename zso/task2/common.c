#include "common.h"
#include "buffers.h"

void release_marked_buffers(struct harddoom_dev *dev)
{
	struct list_head *elem;
	struct list_head *tmp;
	list_for_each_safe(elem, tmp, &dev->list_buffers_release)
	{
		struct doomdev_buffer_header *bufh = container_of(
		    elem, struct doomdev_buffer_header, elem_buffers);
		harddoom_free_buffer(bufh);
	}
}