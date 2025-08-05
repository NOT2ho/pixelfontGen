# pixel font gen
svg -> ttf 는 fontforge 스크립트 돌려야 할 듯

## 자모음 형식
```
g_0 :: [Path]
g_0 = squares $ randomizeSqr seed rfactor (pathrect radius interval [(10,10), (300,20), (320, 30), (300,300)])
```

아직 안만들었음 근데 거의다 한듯  
output 폴더에 있는 건 유니코드 매핑 테스트임. 

## todo
 * 심각한 문제: `line :: Edge -> (Int -> Int)` 이거 함숫값이 x -> y 함수라서 x축에 수직인 선을 표현할 수 없음. 일단 지금은 수직선을 안 긋는 걸로 우회했지만 언젠가 해결해야 할 문제가 분명하다 
 * svg 좌표가 상식적인 방향이 맞는지 확인해야 함. (아닌듯)
 * union에 무슨 문제가 있는 것 같은데 뭔지 모름 (귀찮음 어차피 정상적인 모양에서는 잘됨)
 * 가운데가 뚫린 도형을 svg에서 어케표현하는지알아내야함 (안할것)
 * 이제 그냥 자모 디자인만 하면 끝임... 아마도

# ``는 infix 4, \\\\는 infix 5 이다.
나는 이 사실을 몰랐기 때문에 몇 시간을 낭비했다.

## test..

![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_0.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_1.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_2.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_3.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_4.svg)
![.](https://github.com/NOT2ho/pixelfontGen/blob/main/test_5.svg)

# readme 에 쓸모없는 내용만 가득한가요?
하지만 이미 당신은 끝까지 읽었으며 되돌릴 수 없습니다. 
