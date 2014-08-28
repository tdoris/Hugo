

double mandyloop(int n, double x, double y, double z)
{

  for (int i=0;i<n;i++)
  {
    x = x*x - y*y;
    x = x + 0.01;
    y = 2*x*y;
    y = y+0.1;
    z = z+1;
  }
  return z;
}
