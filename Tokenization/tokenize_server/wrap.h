#include <wchar.h>

wchar_t *alloc_wchar(size_t length);
wchar_t *realloc_wchar(wchar_t *ptr, size_t length);

wchar_t get_wchar(wchar_t *w, size_t idx);
void set_wchar(wchar_t *w, size_t idx, wchar_t value);
