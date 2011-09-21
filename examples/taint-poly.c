$_1_2 char *strcat($_1_2 char *, $_1 const char *);

int main(void)
{
  $tainted char *t;
  char *a, *b;

  a = strcat(a, t);
  b = strcat(b, "foobar");
}
