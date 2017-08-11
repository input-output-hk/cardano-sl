#include <winsock2.h>
#include <iphlpapi.h>
#include <stdio.h>
#include <string.h>
#include <windows.h>

#define MALLOC(x) HeapAlloc(GetProcessHeap(), 0, (x))
#define FREE(x) HeapFree(GetProcessHeap(), 0, (x))

char *getWindowsDefDnsServer(void) {
    FIXED_INFO *pFixedInfo;
    ULONG ulOutBufLen;
    DWORD dwRetVal;

// This function isn't thread-safe because of this 'static' declaration.
// Care is required when calling it.
    static char dnsIPAddress[16] = "";
    char allocationErr[] = "Error allocating memory needed to call GetNetworkParams\n";

    pFixedInfo = (FIXED_INFO *) MALLOC(sizeof (FIXED_INFO));
        if (pFixedInfo == NULL) {
            printf(allocationErr);
            return dnsIPAddress;
        }
        ulOutBufLen = sizeof (FIXED_INFO);

// Make an initial call to GetAdaptersInfo to get the necessary size into the
// ulOutBufLen variable
    if (GetNetworkParams(pFixedInfo, &ulOutBufLen) == ERROR_BUFFER_OVERFLOW) {
        FREE(pFixedInfo);
        pFixedInfo = (FIXED_INFO *) MALLOC(ulOutBufLen);
        if (pFixedInfo == NULL) {
            printf(allocationErr);
            return dnsIPAddress;
        }
    }

    switch (dwRetVal = GetNetworkParams(pFixedInfo, &ulOutBufLen)) {
    case ERROR_BUFFER_OVERFLOW:
        FREE(pFixedInfo);
        pFixedInfo = (FIXED_INFO *) MALLOC(ulOutBufLen);
        if (pFixedInfo == NULL) {
            printf(allocationErr);
        }
    case ERROR_INVALID_PARAMETER:
        printf("An invalid parameter was passed to the function\n");
        break;
    case ERROR_NO_DATA:
        printf("No network parameter information exists for the local computer\n");
        break;
    case ERROR_NOT_SUPPORTED:
        printf("The 'GetNetworkParams function' is not supported by the current "
               "operating system\n");
        break;
    case NO_ERROR:
        strcpy(dnsIPAddress, pFixedInfo->DnsServerList.IpAddress.String);
        break;
    default:
        printf("An error occurred, no IP address returned\n");
    }

    if (pFixedInfo) FREE(pFixedInfo);

    return dnsIPAddress;
}

/*

// Test with 'gcc -o dnsServer -Wall -Werror -pedantic defaultdns.c' on a Windows
// machine.

int main(){
    printf(getWindowsDefDnsServer());
    return 0;
}*/
