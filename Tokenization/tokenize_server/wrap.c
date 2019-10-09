#include <stdlib.h>
#include <wchar.h>

wchar_t *alloc_wchar(size_t length)
{
	return malloc(length * sizeof(wchar_t));
}

wchar_t *realloc_wchar(wchar_t *ptr, size_t length)
{
	return realloc(ptr, length * sizeof(wchar_t));
}

void set_wchar(wchar_t *w, size_t idx, wchar_t value)
{
	w[idx] = value;
}

wchar_t get_wchar(wchar_t *w, size_t idx)
{
	return w[idx];
}