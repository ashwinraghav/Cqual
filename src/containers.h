/* Automatically generated file, do not edit. See containters.def */

/* Copyright (c) 2000-200 The Regents of the University of California. */
/* See the copyright notice in nodetypes.def for distribution restrictions */
#ifndef CONTAINERS_H
#define CONTAINERS_H
#include "AST.h"
#include "cqual.h"
#include "set.h"
typedef struct { set s; } *ty_set;
typedef struct { set_scanner ss; } ty_set_scanner;
extern int cmp_ty(ty, ty);
static inline ty_set empty_ty_set(void) { return (ty_set) empty_set; }
static inline ty_set ty_set_copy(region r, ty_set s) { return (ty_set) set_copy(r, (set) s); }
static inline bool ty_set_empty(ty_set s) { return set_empty((set) s); }
static inline bool ty_set_member(ty_set s, ty elt) { return set_member((set_cmp_fn) cmp_ty, (set) s, (void *) elt); }
static inline int ty_set_size(ty_set s) { return set_size((set) s); }
static inline void ty_set_insert(region r, ty_set *s, ty elt) { set_insert(r, (set_cmp_fn) cmp_ty, FALSE, (set *) s, (void *) elt); }
static inline void ty_set_remove(ty_set *s, ty elt) { set_remove((set_cmp_fn) cmp_ty, FALSE, (set *) s, (void *) elt); }
static inline bool ty_set_subset(ty_set s1, ty_set s2) { return set_subset((set_cmp_fn) cmp_ty, (set) s1, (set) s2); }
static inline ty_set ty_set_union(ty_set s1, ty_set s2) { return (ty_set) set_union((set_cmp_fn) cmp_ty, FALSE, (set) s1, (set) s2); }
static inline bool ty_set_single(ty_set s) { return set_single((set) s); }
static inline void ty_set_sort(ty_set s) { set_sort((set_cmp_fn) cmp_ty, (set) s); }
static inline void ty_set_scan(ty_set s, ty_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline ty ty_set_next(ty_set_scanner *ss) { return (ty) set_next(&ss->ss); }
#define scan_ty_set(var, scanner, set) for (ty_set_scan(set, &scanner), var = ty_set_next(&scanner); var; var = ty_set_next(&scanner))

typedef struct { set s; } *qual_set;
typedef struct { set_scanner ss; } qual_set_scanner;
extern int cmp_qual(qual, qual);
static inline qual_set empty_qual_set(void) { return (qual_set) empty_set; }
static inline qual_set qual_set_copy(region r, qual_set s) { return (qual_set) set_copy(r, (set) s); }
static inline bool qual_set_empty(qual_set s) { return set_empty((set) s); }
static inline bool qual_set_member(qual_set s, qual elt) { return set_member((set_cmp_fn) cmp_qual, (set) s, (void *) elt); }
static inline int qual_set_size(qual_set s) { return set_size((set) s); }
static inline void qual_set_insert(region r, qual_set *s, qual elt) { set_insert(r, (set_cmp_fn) cmp_qual, FALSE, (set *) s, (void *) elt); }
static inline void qual_set_remove(qual_set *s, qual elt) { set_remove((set_cmp_fn) cmp_qual, FALSE, (set *) s, (void *) elt); }
static inline bool qual_set_subset(qual_set s1, qual_set s2) { return set_subset((set_cmp_fn) cmp_qual, (set) s1, (set) s2); }
static inline qual_set qual_set_union(qual_set s1, qual_set s2) { return (qual_set) set_union((set_cmp_fn) cmp_qual, FALSE, (set) s1, (set) s2); }
static inline bool qual_set_single(qual_set s) { return set_single((set) s); }
static inline void qual_set_sort(qual_set s) { set_sort((set_cmp_fn) cmp_qual, (set) s); }
static inline void qual_set_scan(qual_set s, qual_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline qual qual_set_next(qual_set_scanner *ss) { return (qual) set_next(&ss->ss); }
#define scan_qual_set(var, scanner, set) for (qual_set_scan(set, &scanner), var = qual_set_next(&scanner); var; var = qual_set_next(&scanner))

typedef struct { set s; } *qual_set_byname;
typedef struct { set_scanner ss; } qual_set_byname_scanner;
extern int cmp_qual_byname(qual, qual);
static inline qual_set_byname empty_qual_set_byname(void) { return (qual_set_byname) empty_set; }
static inline qual_set_byname qual_set_byname_copy(region r, qual_set_byname s) { return (qual_set_byname) set_copy(r, (set) s); }
static inline bool qual_set_byname_empty(qual_set_byname s) { return set_empty((set) s); }
static inline bool qual_set_byname_member(qual_set_byname s, qual elt) { return set_member((set_cmp_fn) cmp_qual_byname, (set) s, (void *) elt); }
static inline int qual_set_byname_size(qual_set_byname s) { return set_size((set) s); }
static inline void qual_set_byname_insert(region r, qual_set_byname *s, qual elt) { set_insert(r, (set_cmp_fn) cmp_qual_byname, FALSE, (set *) s, (void *) elt); }
static inline void qual_set_byname_remove(qual_set_byname *s, qual elt) { set_remove((set_cmp_fn) cmp_qual_byname, FALSE, (set *) s, (void *) elt); }
static inline bool qual_set_byname_subset(qual_set_byname s1, qual_set_byname s2) { return set_subset((set_cmp_fn) cmp_qual_byname, (set) s1, (set) s2); }
static inline qual_set_byname qual_set_byname_union(qual_set_byname s1, qual_set_byname s2) { return (qual_set_byname) set_union((set_cmp_fn) cmp_qual_byname, FALSE, (set) s1, (set) s2); }
static inline bool qual_set_byname_single(qual_set_byname s) { return set_single((set) s); }
static inline void qual_set_byname_sort(qual_set_byname s) { set_sort((set_cmp_fn) cmp_qual_byname, (set) s); }
static inline void qual_set_byname_scan(qual_set_byname s, qual_set_byname_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline qual qual_set_byname_next(qual_set_byname_scanner *ss) { return (qual) set_next(&ss->ss); }
#define scan_qual_set_byname(var, scanner, set) for (qual_set_byname_scan(set, &scanner), var = qual_set_byname_next(&scanner); var; var = qual_set_byname_next(&scanner))

typedef struct { set s; } *loc_set;
typedef struct { set_scanner ss; } loc_set_scanner;
extern int location_cmp(location, location);
static inline loc_set empty_loc_set(void) { return (loc_set) empty_set; }
static inline loc_set loc_set_copy(region r, loc_set s) { return (loc_set) set_copy(r, (set) s); }
static inline bool loc_set_empty(loc_set s) { return set_empty((set) s); }
static inline bool loc_set_member(loc_set s, location elt) { return set_member((set_cmp_fn) location_cmp, (set) s, (void *) elt); }
static inline int loc_set_size(loc_set s) { return set_size((set) s); }
static inline void loc_set_insert(region r, loc_set *s, location elt) { set_insert(r, (set_cmp_fn) location_cmp, FALSE, (set *) s, (void *) elt); }
static inline void loc_set_remove(loc_set *s, location elt) { set_remove((set_cmp_fn) location_cmp, FALSE, (set *) s, (void *) elt); }
static inline bool loc_set_subset(loc_set s1, loc_set s2) { return set_subset((set_cmp_fn) location_cmp, (set) s1, (set) s2); }
static inline loc_set loc_set_union(loc_set s1, loc_set s2) { return (loc_set) set_union((set_cmp_fn) location_cmp, FALSE, (set) s1, (set) s2); }
static inline bool loc_set_single(loc_set s) { return set_single((set) s); }
static inline void loc_set_sort(loc_set s) { set_sort((set_cmp_fn) location_cmp, (set) s); }
static inline void loc_set_scan(loc_set s, loc_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline location loc_set_next(loc_set_scanner *ss) { return (location) set_next(&ss->ss); }
#define scan_loc_set(var, scanner, set) for (loc_set_scan(set, &scanner), var = loc_set_next(&scanner); var; var = loc_set_next(&scanner))



typedef struct { set s; } *aloctype_set;
typedef struct { set_scanner ss; } aloctype_set_scanner;
static inline aloctype_set empty_aloctype_set(void) { return (aloctype_set) empty_set; }
static inline aloctype_set aloctype_set_copy(region r, aloctype_set s) { return (aloctype_set) set_copy(r, (set) s); }
static inline bool aloctype_set_empty(aloctype_set s) { return set_empty((set) s); }
static inline int aloctype_set_size(aloctype_set s) { return set_size((set) s); }
static inline void aloctype_set_insert(region r, aloctype_set *s, aloctype elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }
static inline bool aloctype_set_subset(aloctype_set s1, aloctype_set s2) { return set_subset(NULL, (set) s1, (set) s2); }
static inline aloctype_set aloctype_set_union(aloctype_set s1, aloctype_set s2) { return (aloctype_set) set_union(NULL, TRUE, (set) s1, (set) s2); }
static inline bool aloctype_set_single(aloctype_set s) { return set_single((set) s); }
static inline void aloctype_set_sort(int (*cmp)(aloctype_set, aloctype_set), aloctype_set s) { set_sort((set_cmp_fn) cmp, (set) s); }
static inline void aloctype_set_remove_dups(int (*cmp)(aloctype, aloctype), aloctype_set s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }
static inline void aloctype_set_scan(aloctype_set s, aloctype_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline aloctype aloctype_set_next(aloctype_set_scanner *ss) { return (aloctype) set_next(&ss->ss); }
#define scan_aloctype_set(var, scanner, set) for (aloctype_set_scan(set, &scanner), var = aloctype_set_next(&scanner); var; var = aloctype_set_next(&scanner))

typedef struct { set s; } *alocreftype_set;
typedef struct { set_scanner ss; } alocreftype_set_scanner;
static inline alocreftype_set empty_alocreftype_set(void) { return (alocreftype_set) empty_set; }
static inline alocreftype_set alocreftype_set_copy(region r, alocreftype_set s) { return (alocreftype_set) set_copy(r, (set) s); }
static inline bool alocreftype_set_empty(alocreftype_set s) { return set_empty((set) s); }
static inline int alocreftype_set_size(alocreftype_set s) { return set_size((set) s); }
static inline void alocreftype_set_insert(region r, alocreftype_set *s, alocreftype elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }
static inline bool alocreftype_set_subset(alocreftype_set s1, alocreftype_set s2) { return set_subset(NULL, (set) s1, (set) s2); }
static inline alocreftype_set alocreftype_set_union(alocreftype_set s1, alocreftype_set s2) { return (alocreftype_set) set_union(NULL, TRUE, (set) s1, (set) s2); }
static inline bool alocreftype_set_single(alocreftype_set s) { return set_single((set) s); }
static inline void alocreftype_set_sort(int (*cmp)(alocreftype_set, alocreftype_set), alocreftype_set s) { set_sort((set_cmp_fn) cmp, (set) s); }
static inline void alocreftype_set_remove_dups(int (*cmp)(alocreftype, alocreftype), alocreftype_set s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }
static inline void alocreftype_set_scan(alocreftype_set s, alocreftype_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline alocreftype alocreftype_set_next(alocreftype_set_scanner *ss) { return (alocreftype) set_next(&ss->ss); }
#define scan_alocreftype_set(var, scanner, set) for (alocreftype_set_scan(set, &scanner), var = alocreftype_set_next(&scanner); var; var = alocreftype_set_next(&scanner))

typedef struct { set s; } *alocfntype_set;
typedef struct { set_scanner ss; } alocfntype_set_scanner;
static inline alocfntype_set empty_alocfntype_set(void) { return (alocfntype_set) empty_set; }
static inline alocfntype_set alocfntype_set_copy(region r, alocfntype_set s) { return (alocfntype_set) set_copy(r, (set) s); }
static inline bool alocfntype_set_empty(alocfntype_set s) { return set_empty((set) s); }
static inline int alocfntype_set_size(alocfntype_set s) { return set_size((set) s); }
static inline void alocfntype_set_insert(region r, alocfntype_set *s, alocfntype elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }
static inline bool alocfntype_set_subset(alocfntype_set s1, alocfntype_set s2) { return set_subset(NULL, (set) s1, (set) s2); }
static inline alocfntype_set alocfntype_set_union(alocfntype_set s1, alocfntype_set s2) { return (alocfntype_set) set_union(NULL, TRUE, (set) s1, (set) s2); }
static inline bool alocfntype_set_single(alocfntype_set s) { return set_single((set) s); }
static inline void alocfntype_set_sort(int (*cmp)(alocfntype_set, alocfntype_set), alocfntype_set s) { set_sort((set_cmp_fn) cmp, (set) s); }
static inline void alocfntype_set_remove_dups(int (*cmp)(alocfntype, alocfntype), alocfntype_set s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }
static inline void alocfntype_set_scan(alocfntype_set s, alocfntype_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline alocfntype alocfntype_set_next(alocfntype_set_scanner *ss) { return (alocfntype) set_next(&ss->ss); }
#define scan_alocfntype_set(var, scanner, set) for (alocfntype_set_scan(set, &scanner), var = alocfntype_set_next(&scanner); var; var = alocfntype_set_next(&scanner))

typedef struct { set s; } *effecttype_set;
typedef struct { set_scanner ss; } effecttype_set_scanner;
static inline effecttype_set empty_effecttype_set(void) { return (effecttype_set) empty_set; }
static inline effecttype_set effecttype_set_copy(region r, effecttype_set s) { return (effecttype_set) set_copy(r, (set) s); }
static inline bool effecttype_set_empty(effecttype_set s) { return set_empty((set) s); }
static inline int effecttype_set_size(effecttype_set s) { return set_size((set) s); }
static inline void effecttype_set_insert(region r, effecttype_set *s, effecttype elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }
static inline bool effecttype_set_subset(effecttype_set s1, effecttype_set s2) { return set_subset(NULL, (set) s1, (set) s2); }
static inline effecttype_set effecttype_set_union(effecttype_set s1, effecttype_set s2) { return (effecttype_set) set_union(NULL, TRUE, (set) s1, (set) s2); }
static inline bool effecttype_set_single(effecttype_set s) { return set_single((set) s); }
static inline void effecttype_set_sort(int (*cmp)(effecttype_set, effecttype_set), effecttype_set s) { set_sort((set_cmp_fn) cmp, (set) s); }
static inline void effecttype_set_remove_dups(int (*cmp)(effecttype, effecttype), effecttype_set s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }
static inline void effecttype_set_scan(effecttype_set s, effecttype_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline effecttype effecttype_set_next(effecttype_set_scanner *ss) { return (effecttype) set_next(&ss->ss); }
#define scan_effecttype_set(var, scanner, set) for (effecttype_set_scan(set, &scanner), var = effecttype_set_next(&scanner); var; var = effecttype_set_next(&scanner))

typedef struct { set s; } *effect_set;
typedef struct { set_scanner ss; } effect_set_scanner;
static inline effect_set empty_effect_set(void) { return (effect_set) empty_set; }
static inline effect_set effect_set_copy(region r, effect_set s) { return (effect_set) set_copy(r, (set) s); }
static inline bool effect_set_empty(effect_set s) { return set_empty((set) s); }
static inline int effect_set_size(effect_set s) { return set_size((set) s); }
static inline void effect_set_insert(region r, effect_set *s, effect elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }
static inline bool effect_set_subset(effect_set s1, effect_set s2) { return set_subset(NULL, (set) s1, (set) s2); }
static inline effect_set effect_set_union(effect_set s1, effect_set s2) { return (effect_set) set_union(NULL, TRUE, (set) s1, (set) s2); }
static inline bool effect_set_single(effect_set s) { return set_single((set) s); }
static inline void effect_set_sort(int (*cmp)(effect_set, effect_set), effect_set s) { set_sort((set_cmp_fn) cmp, (set) s); }
static inline void effect_set_remove_dups(int (*cmp)(effect, effect), effect_set s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }
static inline void effect_set_scan(effect_set s, effect_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline effect effect_set_next(effect_set_scanner *ss) { return (effect) set_next(&ss->ss); }
#define scan_effect_set(var, scanner, set) for (effect_set_scan(set, &scanner), var = effect_set_next(&scanner); var; var = effect_set_next(&scanner))

typedef struct { set s; } *store_edge_set;
typedef struct { set_scanner ss; } store_edge_set_scanner;
static inline store_edge_set empty_store_edge_set(void) { return (store_edge_set) empty_set; }
static inline store_edge_set store_edge_set_copy(region r, store_edge_set s) { return (store_edge_set) set_copy(r, (set) s); }
static inline bool store_edge_set_empty(store_edge_set s) { return set_empty((set) s); }
static inline int store_edge_set_size(store_edge_set s) { return set_size((set) s); }
static inline void store_edge_set_insert(region r, store_edge_set *s, store_edge elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }
static inline bool store_edge_set_subset(store_edge_set s1, store_edge_set s2) { return set_subset(NULL, (set) s1, (set) s2); }
static inline store_edge_set store_edge_set_union(store_edge_set s1, store_edge_set s2) { return (store_edge_set) set_union(NULL, TRUE, (set) s1, (set) s2); }
static inline bool store_edge_set_single(store_edge_set s) { return set_single((set) s); }
static inline void store_edge_set_sort(int (*cmp)(store_edge_set, store_edge_set), store_edge_set s) { set_sort((set_cmp_fn) cmp, (set) s); }
static inline void store_edge_set_remove_dups(int (*cmp)(store_edge, store_edge), store_edge_set s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }
static inline void store_edge_set_scan(store_edge_set s, store_edge_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline store_edge store_edge_set_next(store_edge_set_scanner *ss) { return (store_edge) set_next(&ss->ss); }
#define scan_store_edge_set(var, scanner, set) for (store_edge_set_scan(set, &scanner), var = store_edge_set_next(&scanner); var; var = store_edge_set_next(&scanner))

typedef struct { set s; } *po_set;
typedef struct { set_scanner ss; } po_set_scanner;
static inline po_set empty_po_set(void) { return (po_set) empty_set; }
static inline po_set po_set_copy(region r, po_set s) { return (po_set) set_copy(r, (set) s); }
static inline bool po_set_empty(po_set s) { return set_empty((set) s); }
static inline int po_set_size(po_set s) { return set_size((set) s); }
static inline void po_set_insert(region r, po_set *s, po_info elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }
static inline bool po_set_subset(po_set s1, po_set s2) { return set_subset(NULL, (set) s1, (set) s2); }
static inline po_set po_set_union(po_set s1, po_set s2) { return (po_set) set_union(NULL, TRUE, (set) s1, (set) s2); }
static inline bool po_set_single(po_set s) { return set_single((set) s); }
static inline void po_set_sort(int (*cmp)(po_set, po_set), po_set s) { set_sort((set_cmp_fn) cmp, (set) s); }
static inline void po_set_remove_dups(int (*cmp)(po_info, po_info), po_set s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }
static inline void po_set_scan(po_set s, po_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline po_info po_set_next(po_set_scanner *ss) { return (po_info) set_next(&ss->ss); }
#define scan_po_set(var, scanner, set) for (po_set_scan(set, &scanner), var = po_set_next(&scanner); var; var = po_set_next(&scanner))

typedef struct { set s; } *qual_edge_set;
typedef struct { set_scanner ss; } qual_edge_set_scanner;
static inline qual_edge_set empty_qual_edge_set(void) { return (qual_edge_set) empty_set; }
static inline qual_edge_set qual_edge_set_copy(region r, qual_edge_set s) { return (qual_edge_set) set_copy(r, (set) s); }
static inline bool qual_edge_set_empty(qual_edge_set s) { return set_empty((set) s); }
static inline int qual_edge_set_size(qual_edge_set s) { return set_size((set) s); }
static inline void qual_edge_set_insert(region r, qual_edge_set *s, qual_edge elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }
static inline bool qual_edge_set_subset(qual_edge_set s1, qual_edge_set s2) { return set_subset(NULL, (set) s1, (set) s2); }
static inline qual_edge_set qual_edge_set_union(qual_edge_set s1, qual_edge_set s2) { return (qual_edge_set) set_union(NULL, TRUE, (set) s1, (set) s2); }
static inline bool qual_edge_set_single(qual_edge_set s) { return set_single((set) s); }
static inline void qual_edge_set_sort(int (*cmp)(qual_edge_set, qual_edge_set), qual_edge_set s) { set_sort((set_cmp_fn) cmp, (set) s); }
static inline void qual_edge_set_remove_dups(int (*cmp)(qual_edge, qual_edge), qual_edge_set s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }
static inline void qual_edge_set_scan(qual_edge_set s, qual_edge_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline qual_edge qual_edge_set_next(qual_edge_set_scanner *ss) { return (qual_edge) set_next(&ss->ss); }
#define scan_qual_edge_set(var, scanner, set) for (qual_edge_set_scan(set, &scanner), var = qual_edge_set_next(&scanner); var; var = qual_edge_set_next(&scanner))

typedef struct { set s; } *cond_set;
typedef struct { set_scanner ss; } cond_set_scanner;
static inline cond_set empty_cond_set(void) { return (cond_set) empty_set; }
static inline cond_set cond_set_copy(region r, cond_set s) { return (cond_set) set_copy(r, (set) s); }
static inline bool cond_set_empty(cond_set s) { return set_empty((set) s); }
static inline int cond_set_size(cond_set s) { return set_size((set) s); }
static inline void cond_set_insert(region r, cond_set *s, cond elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }
static inline bool cond_set_subset(cond_set s1, cond_set s2) { return set_subset(NULL, (set) s1, (set) s2); }
static inline cond_set cond_set_union(cond_set s1, cond_set s2) { return (cond_set) set_union(NULL, TRUE, (set) s1, (set) s2); }
static inline bool cond_set_single(cond_set s) { return set_single((set) s); }
static inline void cond_set_sort(int (*cmp)(cond_set, cond_set), cond_set s) { set_sort((set_cmp_fn) cmp, (set) s); }
static inline void cond_set_remove_dups(int (*cmp)(cond, cond), cond_set s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }
static inline void cond_set_scan(cond_set s, cond_set_scanner *ss) { set_scan((set) s, &ss->ss); }
static inline cond cond_set_next(cond_set_scanner *ss) { return (cond) set_next(&ss->ss); }
#define scan_cond_set(var, scanner, set) for (cond_set_scan(set, &scanner), var = cond_set_next(&scanner); var; var = cond_set_next(&scanner))



typedef struct { hash_table ht; } *aloc_int_map;
typedef struct { hash_table_scanner hts; } aloc_int_map_scanner;
typedef struct { hash_table_scanner_sorted htss; } aloc_int_map_scanner_sorted;
extern int hash_aloc(aloc);
extern bool eq_aloc(aloc, aloc);
static inline aloc_int_map make_aloc_int_map(region r, int size) { return (aloc_int_map) make_hash_table(r, size, (hash_fn) hash_aloc, (keyeq_fn) eq_aloc); }
static inline void aloc_int_map_reset(aloc_int_map m) { hash_table_reset((hash_table) m); }
static inline int aloc_int_map_size(aloc_int_map m) { return hash_table_size((hash_table) m); }
static inline bool aloc_int_map_lookup(aloc_int_map m, aloc k, int *d) { return hash_table_lookup((hash_table) m, (hash_key) k, (hash_data *) d); }
static inline bool aloc_int_map_insert(aloc_int_map m, aloc k, int d) { return hash_table_insert((hash_table) m, (hash_key) k, (hash_data) d); }
static inline bool aloc_int_map_remove(aloc_int_map m, aloc k) { return hash_table_remove((hash_table) m, (hash_key) k); }
static inline aloc_int_map aloc_int_map_copy(region r, aloc_int_map m) { return (aloc_int_map) hash_table_copy(r, (hash_table) m); }
static inline aloc_int_map aloc_int_map_map(region r, aloc_int_map m, int (*f)(aloc, int, void *), void *arg) { return (aloc_int_map) hash_table_map(r, (hash_table) m, (hash_map_fn) f, arg); }
static inline void aloc_int_map_scan(aloc_int_map m, aloc_int_map_scanner *ms) { hash_table_scan((hash_table) m, &ms->hts); }
static inline bool aloc_int_map_next(aloc_int_map_scanner *ms, aloc *k, int *d) { return hash_table_next(&ms->hts, (hash_key *) k, (hash_data *) d); }
static inline void aloc_int_map_scan_sorted(aloc_int_map m, int (*f)(aloc, aloc), aloc_int_map_scanner_sorted *mss) { hash_table_scan_sorted((hash_table) m, (keycmp_fn) f, &mss->htss); }
static inline bool aloc_int_map_next_sorted(aloc_int_map_scanner_sorted *ms, aloc *k, int *d) { return hash_table_next_sorted(&ms->htss, (hash_key *) k, (hash_data *) d); }
typedef struct { hash_table ht; } *cell_map;
typedef struct { hash_table_scanner hts; } cell_map_scanner;
typedef struct { hash_table_scanner_sorted htss; } cell_map_scanner_sorted;
extern int hash_aloc(aloc);
extern bool eq_aloc(aloc, aloc);
static inline cell_map make_cell_map(region r, int size) { return (cell_map) make_hash_table(r, size, (hash_fn) hash_aloc, (keyeq_fn) eq_aloc); }
static inline void cell_map_reset(cell_map m) { hash_table_reset((hash_table) m); }
static inline int cell_map_size(cell_map m) { return hash_table_size((hash_table) m); }
static inline bool cell_map_lookup(cell_map m, aloc k, cell *d) { return hash_table_lookup((hash_table) m, (hash_key) k, (hash_data *) d); }
static inline bool cell_map_insert(cell_map m, aloc k, cell d) { return hash_table_insert((hash_table) m, (hash_key) k, (hash_data) d); }
static inline bool cell_map_remove(cell_map m, aloc k) { return hash_table_remove((hash_table) m, (hash_key) k); }
static inline cell_map cell_map_copy(region r, cell_map m) { return (cell_map) hash_table_copy(r, (hash_table) m); }
static inline cell_map cell_map_map(region r, cell_map m, cell (*f)(aloc, cell, void *), void *arg) { return (cell_map) hash_table_map(r, (hash_table) m, (hash_map_fn) f, arg); }
static inline void cell_map_scan(cell_map m, cell_map_scanner *ms) { hash_table_scan((hash_table) m, &ms->hts); }
static inline bool cell_map_next(cell_map_scanner *ms, aloc *k, cell *d) { return hash_table_next(&ms->hts, (hash_key *) k, (hash_data *) d); }
static inline void cell_map_scan_sorted(cell_map m, int (*f)(aloc, aloc), cell_map_scanner_sorted *mss) { hash_table_scan_sorted((hash_table) m, (keycmp_fn) f, &mss->htss); }
static inline bool cell_map_next_sorted(cell_map_scanner_sorted *ms, aloc *k, cell *d) { return hash_table_next_sorted(&ms->htss, (hash_key *) k, (hash_data *) d); }
typedef struct { hash_table ht; } *bitset_bounds_map;
typedef struct { hash_table_scanner hts; } bitset_bounds_map_scanner;
typedef struct { hash_table_scanner_sorted htss; } bitset_bounds_map_scanner_sorted;
extern int bitset_hash(bitset);
extern bool bitset_eq(bitset, bitset);
static inline bitset_bounds_map make_bitset_bounds_map(region r, int size) { return (bitset_bounds_map) make_hash_table(r, size, (hash_fn) bitset_hash, (keyeq_fn) bitset_eq); }
static inline void bitset_bounds_map_reset(bitset_bounds_map m) { hash_table_reset((hash_table) m); }
static inline int bitset_bounds_map_size(bitset_bounds_map m) { return hash_table_size((hash_table) m); }
static inline bool bitset_bounds_map_lookup(bitset_bounds_map m, bitset k, bounds *d) { return hash_table_lookup((hash_table) m, (hash_key) k, (hash_data *) d); }
static inline bool bitset_bounds_map_insert(bitset_bounds_map m, bitset k, bounds d) { return hash_table_insert((hash_table) m, (hash_key) k, (hash_data) d); }
static inline bool bitset_bounds_map_remove(bitset_bounds_map m, bitset k) { return hash_table_remove((hash_table) m, (hash_key) k); }
static inline bitset_bounds_map bitset_bounds_map_copy(region r, bitset_bounds_map m) { return (bitset_bounds_map) hash_table_copy(r, (hash_table) m); }
static inline bitset_bounds_map bitset_bounds_map_map(region r, bitset_bounds_map m, bounds (*f)(bitset, bounds, void *), void *arg) { return (bitset_bounds_map) hash_table_map(r, (hash_table) m, (hash_map_fn) f, arg); }
static inline void bitset_bounds_map_scan(bitset_bounds_map m, bitset_bounds_map_scanner *ms) { hash_table_scan((hash_table) m, &ms->hts); }
static inline bool bitset_bounds_map_next(bitset_bounds_map_scanner *ms, bitset *k, bounds *d) { return hash_table_next(&ms->hts, (hash_key *) k, (hash_data *) d); }
static inline void bitset_bounds_map_scan_sorted(bitset_bounds_map m, int (*f)(bitset, bitset), bitset_bounds_map_scanner_sorted *mss) { hash_table_scan_sorted((hash_table) m, (keycmp_fn) f, &mss->htss); }
static inline bool bitset_bounds_map_next_sorted(bitset_bounds_map_scanner_sorted *ms, bitset *k, bounds *d) { return hash_table_next_sorted(&ms->htss, (hash_key *) k, (hash_data *) d); }
typedef struct { hash_table ht; } *pam_file_map;
typedef struct { hash_table_scanner hts; } pam_file_map_scanner;
typedef struct { hash_table_scanner_sorted htss; } pam_file_map_scanner_sorted;
extern int string_hash(const char *);
extern bool string_eq(const char *, const char *);
static inline pam_file_map make_pam_file_map(region r, int size) { return (pam_file_map) make_hash_table(r, size, (hash_fn) string_hash, (keyeq_fn) string_eq); }
static inline void pam_file_map_reset(pam_file_map m) { hash_table_reset((hash_table) m); }
static inline int pam_file_map_size(pam_file_map m) { return hash_table_size((hash_table) m); }
static inline bool pam_file_map_lookup(pam_file_map m, const char * k, pam_file *d) { return hash_table_lookup((hash_table) m, (hash_key) k, (hash_data *) d); }
static inline bool pam_file_map_insert(pam_file_map m, const char * k, pam_file d) { return hash_table_insert((hash_table) m, (hash_key) k, (hash_data) d); }
static inline bool pam_file_map_remove(pam_file_map m, const char * k) { return hash_table_remove((hash_table) m, (hash_key) k); }
static inline pam_file_map pam_file_map_copy(region r, pam_file_map m) { return (pam_file_map) hash_table_copy(r, (hash_table) m); }
static inline pam_file_map pam_file_map_map(region r, pam_file_map m, pam_file (*f)(const char *, pam_file, void *), void *arg) { return (pam_file_map) hash_table_map(r, (hash_table) m, (hash_map_fn) f, arg); }
static inline void pam_file_map_scan(pam_file_map m, pam_file_map_scanner *ms) { hash_table_scan((hash_table) m, &ms->hts); }
static inline bool pam_file_map_next(pam_file_map_scanner *ms, const char * *k, pam_file *d) { return hash_table_next(&ms->hts, (hash_key *) k, (hash_data *) d); }
static inline void pam_file_map_scan_sorted(pam_file_map m, int (*f)(const char *, const char *), pam_file_map_scanner_sorted *mss) { hash_table_scan_sorted((hash_table) m, (keycmp_fn) f, &mss->htss); }
static inline bool pam_file_map_next_sorted(pam_file_map_scanner_sorted *ms, const char * *k, pam_file *d) { return hash_table_next_sorted(&ms->htss, (hash_key *) k, (hash_data *) d); }
typedef struct { hash_table ht; } *pam_click_map;
typedef struct { hash_table_scanner hts; } pam_click_map_scanner;
typedef struct { hash_table_scanner_sorted htss; } pam_click_map_scanner_sorted;
extern int string_hash(const char *);
extern bool string_eq(const char *, const char *);
static inline pam_click_map make_pam_click_map(region r, int size) { return (pam_click_map) make_hash_table(r, size, (hash_fn) string_hash, (keyeq_fn) string_eq); }
static inline void pam_click_map_reset(pam_click_map m) { hash_table_reset((hash_table) m); }
static inline int pam_click_map_size(pam_click_map m) { return hash_table_size((hash_table) m); }
static inline bool pam_click_map_lookup(pam_click_map m, const char * k, pam_click *d) { return hash_table_lookup((hash_table) m, (hash_key) k, (hash_data *) d); }
static inline bool pam_click_map_insert(pam_click_map m, const char * k, pam_click d) { return hash_table_insert((hash_table) m, (hash_key) k, (hash_data) d); }
static inline bool pam_click_map_remove(pam_click_map m, const char * k) { return hash_table_remove((hash_table) m, (hash_key) k); }
static inline pam_click_map pam_click_map_copy(region r, pam_click_map m) { return (pam_click_map) hash_table_copy(r, (hash_table) m); }
static inline pam_click_map pam_click_map_map(region r, pam_click_map m, pam_click (*f)(const char *, pam_click, void *), void *arg) { return (pam_click_map) hash_table_map(r, (hash_table) m, (hash_map_fn) f, arg); }
static inline void pam_click_map_scan(pam_click_map m, pam_click_map_scanner *ms) { hash_table_scan((hash_table) m, &ms->hts); }
static inline bool pam_click_map_next(pam_click_map_scanner *ms, const char * *k, pam_click *d) { return hash_table_next(&ms->hts, (hash_key *) k, (hash_data *) d); }
static inline void pam_click_map_scan_sorted(pam_click_map m, int (*f)(const char *, const char *), pam_click_map_scanner_sorted *mss) { hash_table_scan_sorted((hash_table) m, (keycmp_fn) f, &mss->htss); }
static inline bool pam_click_map_next_sorted(pam_click_map_scanner_sorted *ms, const char * *k, pam_click *d) { return hash_table_next_sorted(&ms->htss, (hash_key *) k, (hash_data *) d); }
typedef struct { hash_table ht; } *pam_buffer_map;
typedef struct { hash_table_scanner hts; } pam_buffer_map_scanner;
typedef struct { hash_table_scanner_sorted htss; } pam_buffer_map_scanner_sorted;
extern int string_hash(const char *);
extern bool string_eq(const char *, const char *);
static inline pam_buffer_map make_pam_buffer_map(region r, int size) { return (pam_buffer_map) make_hash_table(r, size, (hash_fn) string_hash, (keyeq_fn) string_eq); }
static inline void pam_buffer_map_reset(pam_buffer_map m) { hash_table_reset((hash_table) m); }
static inline int pam_buffer_map_size(pam_buffer_map m) { return hash_table_size((hash_table) m); }
static inline bool pam_buffer_map_lookup(pam_buffer_map m, const char * k, pam_click_map *d) { return hash_table_lookup((hash_table) m, (hash_key) k, (hash_data *) d); }
static inline bool pam_buffer_map_insert(pam_buffer_map m, const char * k, pam_click_map d) { return hash_table_insert((hash_table) m, (hash_key) k, (hash_data) d); }
static inline bool pam_buffer_map_remove(pam_buffer_map m, const char * k) { return hash_table_remove((hash_table) m, (hash_key) k); }
static inline pam_buffer_map pam_buffer_map_copy(region r, pam_buffer_map m) { return (pam_buffer_map) hash_table_copy(r, (hash_table) m); }
static inline pam_buffer_map pam_buffer_map_map(region r, pam_buffer_map m, pam_click_map (*f)(const char *, pam_click_map, void *), void *arg) { return (pam_buffer_map) hash_table_map(r, (hash_table) m, (hash_map_fn) f, arg); }
static inline void pam_buffer_map_scan(pam_buffer_map m, pam_buffer_map_scanner *ms) { hash_table_scan((hash_table) m, &ms->hts); }
static inline bool pam_buffer_map_next(pam_buffer_map_scanner *ms, const char * *k, pam_click_map *d) { return hash_table_next(&ms->hts, (hash_key *) k, (hash_data *) d); }
static inline void pam_buffer_map_scan_sorted(pam_buffer_map m, int (*f)(const char *, const char *), pam_buffer_map_scanner_sorted *mss) { hash_table_scan_sorted((hash_table) m, (keycmp_fn) f, &mss->htss); }
static inline bool pam_buffer_map_next_sorted(pam_buffer_map_scanner_sorted *ms, const char * *k, pam_click_map *d) { return hash_table_next_sorted(&ms->htss, (hash_key *) k, (hash_data *) d); }
typedef struct { hash_table ht; } *pam_overlay_map;
typedef struct { hash_table_scanner hts; } pam_overlay_map_scanner;
typedef struct { hash_table_scanner_sorted htss; } pam_overlay_map_scanner_sorted;
extern int location_hash(location);
extern bool location_eq(location, location);
static inline pam_overlay_map make_pam_overlay_map(region r, int size) { return (pam_overlay_map) make_hash_table(r, size, (hash_fn) location_hash, (keyeq_fn) location_eq); }
static inline void pam_overlay_map_reset(pam_overlay_map m) { hash_table_reset((hash_table) m); }
static inline int pam_overlay_map_size(pam_overlay_map m) { return hash_table_size((hash_table) m); }
static inline bool pam_overlay_map_lookup(pam_overlay_map m, location k, pam_overlay *d) { return hash_table_lookup((hash_table) m, (hash_key) k, (hash_data *) d); }
static inline bool pam_overlay_map_insert(pam_overlay_map m, location k, pam_overlay d) { return hash_table_insert((hash_table) m, (hash_key) k, (hash_data) d); }
static inline bool pam_overlay_map_remove(pam_overlay_map m, location k) { return hash_table_remove((hash_table) m, (hash_key) k); }
static inline pam_overlay_map pam_overlay_map_copy(region r, pam_overlay_map m) { return (pam_overlay_map) hash_table_copy(r, (hash_table) m); }
static inline pam_overlay_map pam_overlay_map_map(region r, pam_overlay_map m, pam_overlay (*f)(location, pam_overlay, void *), void *arg) { return (pam_overlay_map) hash_table_map(r, (hash_table) m, (hash_map_fn) f, arg); }
static inline void pam_overlay_map_scan(pam_overlay_map m, pam_overlay_map_scanner *ms) { hash_table_scan((hash_table) m, &ms->hts); }
static inline bool pam_overlay_map_next(pam_overlay_map_scanner *ms, location *k, pam_overlay *d) { return hash_table_next(&ms->hts, (hash_key *) k, (hash_data *) d); }
static inline void pam_overlay_map_scan_sorted(pam_overlay_map m, int (*f)(location, location), pam_overlay_map_scanner_sorted *mss) { hash_table_scan_sorted((hash_table) m, (keycmp_fn) f, &mss->htss); }
static inline bool pam_overlay_map_next_sorted(pam_overlay_map_scanner_sorted *ms, location *k, pam_overlay *d) { return hash_table_next_sorted(&ms->htss, (hash_key *) k, (hash_data *) d); }
#endif
