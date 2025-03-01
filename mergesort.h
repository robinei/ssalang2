#include <string.h>

#ifndef NAME
#error Define NAME before including this header
#endif

#ifndef TYPE
#error Define TYPE before including this header
#endif

#ifdef INTERFACE_ONLY
#undef INTERFACE_ONLY
void NAME(TYPE *arr, int left, int right);
#else

#ifndef COMPARE
#error Define COMPARE before including this header
#endif

// The subarray to be sorted is in the index range [left-right]
// tempArr must be at least right-left+1 elements long
void NAME(TYPE *arr, int left, int right, TYPE *tempArr) {
    if (left >= right) {
        return;
    }

    // Calculate the midpoint
    int mid = left + (right - left) / 2;

    // Sort first and second halves
    NAME(arr, left, mid, tempArr);
    NAME(arr, mid + 1, right, tempArr);

    // Merges two subarrays of arr[].
    // First subarray is arr[left..mid]
    // Second subarray is arr[mid+1..right]
    int i, j, k;
    int n1 = mid - left + 1;
    int n2 = right - mid;

    // Copy data to temporary arrays
    TYPE *leftArr = tempArr;
    TYPE *rightArr = tempArr + n1;
    memcpy(leftArr, arr + left, sizeof(TYPE) * n1);
    memcpy(rightArr, arr + mid + 1, sizeof(TYPE) * n2);

    // Merge the temporary arrays back into arr[left..right]
    i = 0;
    j = 0;
    k = left;
    while (i < n1 && j < n2) {
        if ((COMPARE(leftArr[i], rightArr[j])) <= 0) {
            arr[k] = leftArr[i];
            i++;
        }
        else {
            arr[k] = rightArr[j];
            j++;
        }
        k++;
    }

    // Copy the remaining elements of leftArr[], if any
    while (i < n1) {
        arr[k] = leftArr[i];
        i++;
        k++;
    }

    // Copy the remaining elements of rightArr[], if any
    while (j < n2) {
        arr[k] = rightArr[j];
        j++;
        k++;
    }
}

#undef COMPARE
#endif

#undef NAME
#undef TYPE
