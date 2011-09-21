typedef int lock_t;

lock_t lock;

int main(void)
{
  lock = ($unlocked lock_t) 0;
  lock;
  lock = ($locked lock_t) 1;
  lock;
  lock = ($unlocked lock_t) 0;
  lock;
}
