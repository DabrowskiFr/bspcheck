int pid();

int h (int * x, int y){
  *x = y;
  return x;
}

int g(int x){
  int y;
  if (pid()){
    y = h(&x, pid());
    return x;
  }
  else return pid();
}

int f(){
  int x = 0;
  if (pid()){
    if (g(1)){
      x = 2;
      x = x + 2;
    }
  }
  h(&x, pid());
  if (x) x=x+1; else x=x+1;
  // Should also say which dependences prevent textual alignement
  return x+1;
}

int main(){
  return f();
}
