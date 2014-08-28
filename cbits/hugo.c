

double mandyloop(int n, double x, double y, double z)
{
  double tx,ty;
  for (int i=0;i<n;i++)
  {
    tx = x + 0.01;
    ty = y + 0.1;
    x = tx*tx - ty*ty ;
    y = 2*tx*ty;
    z = z+1;
  }
  return x;
}
