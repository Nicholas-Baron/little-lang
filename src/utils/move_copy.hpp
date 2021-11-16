#ifndef MOVE_COPY_HPP
#define MOVE_COPY_HPP

#define movable(type)        \
    type(type &&) = default; \
    type & operator=(type &&) = default

#define non_copyable(type)       \
    type(const type &) = delete; \
    type & operator=(const type &) = delete

#endif
