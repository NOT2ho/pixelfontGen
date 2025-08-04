# pixel font gen
svg -> ttf 는 다른 프로그램 쓸 거임  

## 자모음 형식
```
gi0 :: [Path]
gi0 = squares $ randomizeSqr seed rfactor (pathrect 91 79 [(10,10), (300,20), (320, 30), (300,300)])
```

아직 안만들었음 근데 거의다 한듯

## todo
 * svg 좌표가 상식적인 방향이 맞는지 확인해야 함. (아닌듯)
 * union에 무슨 문제가 있는 것 같은데 뭔지 모름
 * 조합해야함

# ``는 infix 4, \\\\는 infix 5 이다.
나는 이 사실을 몰랐기 때문에 몇 시간을 낭비했다.

## test..

![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_0.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_1.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_2.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_3.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_4.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_5.svg)
