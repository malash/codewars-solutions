function dontGiveMeFive(start, end)
{
  let result = 0;
  for (let i = start; i <= end; i++) {
    if ((i + '').indexOf('5') === -1) {
      result++;
    }
  }
  return result;
}
