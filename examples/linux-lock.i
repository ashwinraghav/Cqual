typedef struct {
  volatile unsigned int lock;
} spinlock_t;
static spinlock_t rtc_lock = ($unlocked spinlock_t) { 1 };
int main(void) {
  rtc_lock;
  (({assert_type(*(&rtc_lock), $unlocked spinlock_t); change_type(*(&rtc_lock), $locked spinlock_t);}));
  rtc_lock;
  (({assert_type(*(&rtc_lock), $locked spinlock_t); change_type(*(&rtc_lock), $unlocked spinlock_t);}));
  rtc_lock;
}
