typedef struct {
  volatile unsigned int lock;
} spinlock_t;

void spin_lock($unlocked spinlock_t *lock) {
  change_type(*lock, $locked spinlock_t);
}

void spin_unlock($locked spinlock_t *lock) {
  change_type(*lock, $unlocked spinlock_t);
}

$unlocked spinlock_t rtc_lock;

int main(void) {
  rtc_lock;
  spin_lock(&rtc_lock);
  rtc_lock;
  spin_unlock(&rtc_lock);
  rtc_lock;
}
