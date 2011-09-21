int printf(const char * format, ...);
void pr_year(char * year) {
   printf("The year is 19%s", year);
}
int main() {
   pr_year("99");
   pr_year("2000");
   return 0;
} 
