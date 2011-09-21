typedef struct {
  volatile unsigned int lock;
} spinlock_t;

#define SPIN_LOCK_UNLOCKED ($unlocked spinlock_t) { 1 }
#define spin_lock(x) (({assert_type(*(x), $unlocked spinlock_t); change_type(*(x), $locked spinlock_t);}))
#define spin_unlock(x) (({assert_type(*(x), $locked spinlock_t); change_type(*(x), $unlocked spinlock_t);}))

static spinlock_t rtc_lock = SPIN_LOCK_UNLOCKED;

void bar(void) {
  spin_lock(&rtc_lock);
  spin_unlock(&rtc_lock);
}

void foo(void) {
  spin_lock(&rtc_lock);
  bar();
  spin_unlock(&rtc_lock);
}
