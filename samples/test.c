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

int k(int * x, int *z, int y){

  *x = bsp_pid();
  *z = *x;
  y  = bsp_pid();
  return 1;
}


int f(){
  int x = 0;
  int y = 0;
  int z = 0;
  if (g()){
      x = 2;
      x = x + 2;
  }
  k(&y,&z, x);
  if (y) x=x+1; else x=x+2;
  return bsp_pid();
}

int main(){
  int x = f();  
  if (x) return 1;
  else {
    int y = g();
    if (y) return 1;
    else return 2;
  }
}
