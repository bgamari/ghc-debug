#include <Rts.h>
#include "rts/storage/Heap.h"

StgArrBytes *heap_view_closurePtrsAsWords(Capability *cap, StgClosure *closure) {
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(closure));

    StgWord size = heap_view_closureSize(closure);

    // First collect all pointers here, with the comfortable memory bound
    // of the whole closure. Afterwards we know how many pointers are in
    // the closure and then we can allocate space on the heap and copy them
    // there
    StgClosure *ptrs[size];
    StgWord nptrs = collect_pointers(closure, size, ptrs);
    StgArrBytes *arr =
        (StgArrBytes *)allocate(cap, sizeofW(StgArrBytes) + nptrs);
    TICK_ALLOC_PRIM(sizeofW(StgArrBytes), nptrs, 0);
    SET_HDR(arr, &stg_ARR_WORDS_info, ((CapabilityPublic *)cap)->r.rCCCS);
    arr->bytes = sizeof(StgWord) * nptrs;

    for (StgWord i = 0; i<nptrs; i++) {
        arr->payload[i] = (StgWord)ptrs[i];
    }

    return arr;
}
