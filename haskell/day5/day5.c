#include <stdio.h>
#include <stdlib.h>

#define CRATE_ROWS 9
#define CRATE_MAX_STACK 8

void read_crate_line(char crates[CRATE_ROWS][CRATE_MAX_STACK], FILE* pFile) {
  char buf[37];
  for (int i=1; i <= CRATE_MAX_STACK; i++) {
    fgets(buf, 37, pFile);
    for (int j=0; j < CRATE_ROWS; j++) {
        crates[j][CRATE_MAX_STACK - i] = buf[4*j + 1];
    }
  } 
}


void read_step(char*)

void do_step(char crates[CRATE_ROWS][CRATE_MAX_STACK+1], char* step) {

}

int main() {
  char crates[CRATE_ROWS][CRATE_MAX_STACK];

  read_crates(crates, stdin);

  for (int row = 0; row < CRATE_ROWS; row++) {
    printf("%.8s\n", crates[row]);
  }

  // while (fgets(line, MAX_L, input)) {
  //   if (line[0] == '\n') {
  //     sums[line_num] = sum;
  //     sum = 0;
  //     line_num++;
  //   }
  //   sum += atoi(line);
  // }
  //
  // printf("The elf is carrying %d\n calories!", 5);
}