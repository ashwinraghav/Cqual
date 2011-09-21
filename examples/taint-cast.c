int main(void)
{
  $tainted char *a;
  void *b;
  char *c;

  b = (void *) a;
  c = (char *) b;
}
