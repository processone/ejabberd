// Just a quick little utility to dump groups in an easier to read format

#include "uni_data.c"
#include <stdio.h>

void main(void) {
    const char format[] = "%10X %15u %2hhu %3hhu %3hhu %3hhu %3hhu %3hhu %3hhu %2hhu\n";
    const char derpformat[] = "%10s %15s %2s %3s %3s %3s %3s %3s %3s %2s\n";
    int ngroups = sizeof(groups)/sizeof(int);
    int i;
    printf(derpformat,"Codepoint","Attrnum","AC","C.1","C.2","B.1","D.1","D.2","XNP","MC");
    for(i=0;i<ngroups;++i) {
        int attrs = groups[i];
        printf(format,i,attrs,
                attrs & ACMask,
                attrs & C11Mask,
                attrs & C21Mask,
                attrs & B1Mask,
                attrs & D1Mask,
                attrs & D2Mask,
                attrs & XNPMask,
                attrs & MCMask);
    }
}
