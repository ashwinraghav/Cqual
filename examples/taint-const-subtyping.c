void f(char *x);
void g(const char *y);

int main(void)
{
  $tainted char *t;
  char *a;
  char *b;

  f(t);
  f(a);

  g(t);
  g(b);

}
