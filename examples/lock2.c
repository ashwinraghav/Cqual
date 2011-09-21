typedef int lock_t;

lock_t lock;

int main(void)
{
  change_type(lock, $unlocked lock_t);
  lock;
  change_type(lock, $locked lock_t);
  lock;
  change_type(lock, $unlocked lock_t);
  lock;
}
