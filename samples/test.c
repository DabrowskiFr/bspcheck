int pid();

int h (int * x, int y){
  *x = y;
  return 1;
}

int g(int x){
  return pid();
}

int f(){
  int x = 0;
  int y;
  int z;
  int t;
  int u;
  t = pid();
  u = t;
  if (pid()){ x = x + 2;}
  y = h(&x, pid());
  z = g(1);
  
  return y;
}

int main(){
  return f();
}
