#include <sys/resource.h>
#include <stdio.h>

/*
* To call this C function from Fortran, use this interface:
*
* interface
*   subroutine get_maxrss() result(maxrss) bind(c)
*       use iso_c_binding, only : c_long
*       integer (c_long), intent(out) :: maxrss
*   end subroutine get_maxrss
* end interface
*/
void get_maxrss(long long *maxrss_bytes) {

    long long conversion;
    struct rusage usage;

#if  defined(__APPLE_) || defined(__MACH__)
    conversion = 1ll;
#elif __linux__
    // Linux's getrusage is in kilobytes, so convert it to bytes
    conversion = 1024ll;
#else  
    conversion = 1024ll;
#endif


    getrusage(RUSAGE_SELF, &usage);
    *maxrss_bytes = usage.ru_maxrss * conversion;    
    printf("Conversion: %lld max: %lld\n", conversion, *maxrss_bytes);
}
