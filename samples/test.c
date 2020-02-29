int bsp_pid();

int h (int * x, int y){
  *x = y;
  return 1;
}



int g(){
  int x;
  h(&x, bsp_pid());
  if (x){
    return bsp_pid();
  }
  else return 0;
}

int f(){
  int x = 0;
  if (g()){
      x = 2;
      x = x + 2;
  }
  return x;
}

int main(){
  int x = f();

  if (x) return 1;
  else return 2;

}
