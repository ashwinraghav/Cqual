typedef struct {
  volatile unsigned int lock;
} spinlock_t;
static spinlock_t rtc_lock = ($unlocked spinlock_t) { 1 };
void bar(void) {
  (({assert_type(*(&rtc_lock), $unlocked spinlock_t); change_type(*(&rtc_lock), $locked spinlock_t);}));
  (({assert_type(*(&rtc_lock), $locked spinlock_t); change_type(*(&rtc_lock), $unlocked spinlock_t);}));
}
void foo(void) {
  (({assert_type(*(&rtc_lock), $unlocked spinlock_t); change_type(*(&rtc_lock), $locked spinlock_t);}));
  bar();
  (({assert_type(*(&rtc_lock), $locked spinlock_t); change_type(*(&rtc_lock), $unlocked spinlock_t);}));
}
