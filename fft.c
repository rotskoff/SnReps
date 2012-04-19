/* An implementation of the FFT in C. */

// dir 1 is the forward FFT
// dir -1 is the inverse FFT
short FFT(short int dir, long m, double *x, double *y) {
  long n,i,i1,j,k,i2,l,l1,l2;
  double c1,c2,tx,ty,t1,t2,u1,u2,z;

  /* Calculate the number of points */
  n = 1;
  for (i=0;i<m;i++) {
    n *= 2;
  }

  // bit reversal
  // what's going on here?
  i2 = n >> 1;
  j = 0;
  for (i=0;i<n-1;i++) {
    if (i<j) {
      tx = x[i];
      ty = y[i];
      x[i] = x[j];
      y[i] = y[j];
      x[j] = tx;
      y[j] = ty;
    }
    k = i2;
    while (k <= j) {
      j -= k;
      k >>= 1;
    }
    j += k;
  }

  /* Now, compute the FFT */
  c1 = -1.0;
  c2 = 0.0;
  l2 = 1;
  for (l=0;l<m;l++) {
    l1 = l2;
    l2 <<= 1;
    u1 = 1.0;
    u2 = 0.0;
    for (j=0;j<l1;j++) {
      i1 = i + l1;
      t1 = u1 * x[i1] - u2 * x[i2];
      t2 = u1 * y[i1] + u2 * y[i2];
      x[i1] = x[i] - t1;
      y[i1] = y[i] - t2;
      x[i] += t1;
      y[i] += t2;
    }
    c2 = sqrt((1.0 - c1)/2.0);
    if (dir == 1)
      c2 = -c2;
    c1 = sqrt((1.0 + c1)/2.0);
  }

  if (dir==1) {
    for (i=0;i<n;i++) {
      x[i] /= n;
      y[i] /= n;
    }
  }

  return(TRUE);
}
