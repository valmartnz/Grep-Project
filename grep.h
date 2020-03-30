#include <stdio.h>

void commands(void); unsigned int *address(void);  int advance(char *lp, char *ep);
int append(int (*f)(void), unsigned int *a);  int backref(int i, char *lp);
void blkio(int b, char *buf, long (*iofcn)(int, void*, unsigned long));
int cclass(char *set, int c, int af);  void compile(int eof);
void error(char *s);  int execute(unsigned int *addr);  void exfile(void);
void filename(int comm); char *getblock(unsigned int atl, int iof); int getchr(void);
int getfile(void);  char *getline_blk(unsigned int tl);  int getnum(void); void global(int k);
void init(void);  void newline(void);  void nonzero(void);  void onhup(int n); void onintr(int n);  void print(void);
void putchr_(int ac); void putd(void);  void putfile(void);  int putline(void);  void puts_(char *sp);
void quit(int n); void reverse(unsigned int *a1, unsigned int *a2); void setwide(void);  void setnoaddr(void);
void squeeze(int); void usage(void); void readfile(const char* c); void search(const char* c); void search_file(const char* filename, const char* searchfor);
void greperror(char);  void grepline(void); void compile_pattern(const char *pat); void process(const char *name, FILE *fp);
void usage(void);
