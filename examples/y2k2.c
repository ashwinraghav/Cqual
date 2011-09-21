int printf(const char * format, ...);
void pr_year(char *$YY year) {
   printf("The year is 19%s", year);
}
int main() {
   pr_year((char *$YY) "99");
   pr_year((char *$YYYY) "2000");
   return 0;
} 
