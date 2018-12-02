##기업리뷰 수집 - 잡 플래닛


# 필요한 라이브러리를 불러옵니다. 
library(httr)

library(rvest)
library(tidyverse)

# 로그인 화면의 URI를 복사하여 URI 객체에 지정합니다.
URI <- 'https://www.jobplanet.co.kr/users/sign_in'

# 로그인 정보를 이용하여 HTTP 요청을 합니다. 
resp <- POST(url = URI,
             body = list('user[email]' = 'shoseo_kjs@naver.com',
                         'user[password]' = 'gocis21103'))

# 응답 상태코드를 확인합니다. 200이면 정상입니다.
status_code(x = resp)
## [1] 200

# 쿠키만 수집하여 myCookies 객체에 할당합니다. 
# 앞으로 HTTP 요청할 때 myCookies를 활용하면 로그인 상태로 HTML을 받을 수 있습니다. 
myCookies <- set_cookies(.cookies = unlist(x = cookies(x = resp)))


#수집할 기업명을 지정합니다.
#compNm <- '삼성화재해상보험'
compNm <- '카카오'

# 잡플래닛 웹 페이지에서 삼성화재 기업리뷰를 확인할 수 있는 URI을 복사하여 붙입니다.
# 분명 웹 브라우저에서는 '삼성화재해상보험'이라고 한글로 보였을 것입니다. 
# 그런데 RStudio로 붙여넣기 하면 이상한 코드로 바뀝니다. 이것은 'URL인코딩'입니다. 
# 한글은 사람이 읽을 수 있지만 컴퓨터는 읽지 못하므로 컴퓨터가 읽을 수 있도록 변환해준 것입니다. 

#https://www.jobplanet.co.kr/companies/30056/reviews/%EC%82%BC%EC%84%B1%ED%99%94%EC%9E%AC%ED%95%B4%EC%83%81%EB%B3%B4%ED%97%98

#URI <- 'https://www.jobplanet.co.kr/companies/30056/reviews/%EC%82%BC%EC%84%B1%ED%99%94%EC%9E%AC%ED%95%B4%EC%83%81%EB%B3%B4%ED%97%98'
URI <- 'https://www.jobplanet.co.kr/companies/93880/reviews/%EC%B9%B4%EC%B9%B4%EC%98%A4'

# 쿠키를 이용하여 로그인 상태 가장하여 HTTP 요청을 합니다.
resp <- GET(url = URI, config = list(cookies = myCookies))

# 응답 상태코드를 확인합니다.
status_code(x = resp)
# [1] 200

# 기업리뷰 수를 추출합니다. (2018년 12월 1일 기준 : 279건)
# 아래 코드에서 CSS Selector를 확인하려면 크롬 개발자도구를 이용해야 합니다. 
# 이 과정을 설명하려면 내용이 길어지므로 이번 포스팅에서는 생략합니다. 

#F12 - copy - Copy Selector
#viewCompaniesMenu > ul > li.viewReviews > a > span

reviewCnt <- resp %>% 
  read_html() %>% 
  html_nodes(css = 'li.viewReviews > a > span') %>% 
  html_text() %>% 
  as.numeric()

# 결과를 출력합니다.
print(x = reviewCnt)
## [1] 283


#현재 웹브라우저에 있는 5개의 리뷰 크롤링

# 잡플래닛에 저장된 회사이름을 추출합니다.
resp %>% read_html() %>% html_node(css = 'h2.tit') %>% html_text()
## [1] "삼성화재해상보험(주)"

# 기업리뷰가 포함된 가장 상위의 HTML 요소를 지정합니다. (5개만 추출됨)
items <- resp %>% read_html() %>% html_nodes(css = 'section.content_ty4')
length(x = items)
## [1] 5

# 회사코드와 리뷰코드를 추출합니다. (잡플래닛에서 부여)
items %>% html_attr(name = 'data-company_id')

#모든 페이지의 리뷰 크롤링

#사용자 정의 함수
# CSS Selector로 텍스트만 수집하는 함수를 생성합니다.
getHtmlText <- function(x, css) {
  
  result <- x %>% 
    html_node(css = css) %>% 
    html_text()
  
  return(result)
}


# CSS Selector로 별점을 수집하는 함수를 생성합니다.
getHtmlRate <- function(x, css, name) {
  
  result <- x %>% 
    html_node(css = css) %>% 
    html_attr(name = name) %>% 
    str_remove_all(pattern = '(width:)|(%;)') %>% 
    as.numeric()
  
  return(result)
}


# 개별 기업리뷰를 수집하고 데이터 프레임으로 반환하는 함수를 생성합니다.
getData <- function(x) {
  
  # 기업리뷰를 포함하는 HTML 요소를 추출하여 items 객체에 할당합니다.
  items <- x %>% read_html() %>% html_nodes(css = 'section.content_ty4')
  
  # 웹 데이터를 수집하여 df 객체에 할당합니다. 
  df <- 
    data.frame(
      회사이름 = x %>% read_html() %>% html_node(css = 'h2.tit') %>% html_text(),
      회사코드 = items %>% html_attr(name = 'data-company_id'),
      리뷰코드 = items %>% html_attr(name = 'data-content_id'),
      직종구분 = getHtmlText(x = items, css = 'div.content_top_ty2 span:nth-child(2)'),
      재직상태 = getHtmlText(x = items, css = 'div.content_top_ty2 span:nth-child(4)'),
      근무지역 = getHtmlText(x = items, css = 'div.content_top_ty2 span:nth-child(6)'),
      등록일자 = getHtmlText(x = items, css = 'div.content_top_ty2 span.txt2'),
      별점평가 = getHtmlRate(x = items, css = 'div.star_score', name = 'style'),
      승진기회 = getHtmlRate(x = items, css = 'dl dd:nth-child(3) div div', name = 'style'),
      복지급여 = getHtmlRate(x = items, css = 'dl dd:nth-child(5) div div', name = 'style'),
      워라밸   = getHtmlRate(x = items, css = 'dl dd:nth-child(7) div div', name = 'style'),
      사내문화 = getHtmlRate(x = items, css = 'dl dd:nth-child(9) div div', name = 'style'),
      경영진   = getHtmlRate(x = items, css = 'dl dd:nth-child(11) div div', name = 'style'),
      기업장점 = getHtmlText(x = items, css = 'dl dd:nth-child(2) span'),
      기업단점 = getHtmlText(x = items, css = 'dl dd:nth-child(4) span'),
      바라는점 = getHtmlText(x = items, css = 'dl dd:nth-child(6) span'),
      성장예상 = getHtmlText(x = items, css = 'p.etc_box strong'),
      추천여부 = getHtmlText(x = items, css = 'p.txt.recommend.etc_box')
    )
  
  return(df)
}

## [1] "30056" "30056" "30056" "30056" "30056"



items %>% html_attr(name = 'data-content_id')
## [1] "849559" "847215" "847067" "846461" "840800"



# 개별 기업리뷰 상단에 있는 직종구분, 재직상태, 근무지역, 등록일자를 추출합니다.
items %>% html_nodes(css = 'div.content_top_ty2 span:nth-child(2)') %>% html_text()
## [1] "영업/제휴"        "금융/재무"        "경영/기획/컨설팅" "영업/제휴"        "금융/재무"

items %>% html_nodes(css = 'div.content_top_ty2 span:nth-child(4)') %>% html_text()
## [1] "현직원" "전직원" "전직원" "전직원" "현직원"

items %>% html_nodes(css = 'div.content_top_ty2 span:nth-child(6)') %>% html_text()
## [1] "서울" "서울" "서울" "부산" "서울"

items %>% html_nodes(css = 'div.content_top_ty2 span.txt2') %>% html_text()
## "2018/11/8"  "2018/11/2"  "2018/10/24" "2018/10/21" "2018/10/16"


# 개별 기업리뷰 왼쪽에 있는 별점(총점, 승진기회, 복지급여, 워라밸, 사내문화, 경영진)을 추출합니다.
# 꺽쇠 사이에 텍스트로 존재하는 대신 HTML 요소의 속성값으로 존재하고 있으며, 
# 'width:100%'와 같은 형태이므로 나중에 정규식을 활용하여 숫자만 추출해야 합니다.
items %>% html_nodes(css = 'div.star_score') %>% html_attr(name = 'style')
## "width:60%;" "width:60%;" "width:60%;" "width:80%;" "width:80%;"

items %>% html_nodes(css = 'dl dd:nth-child(3) div div') %>% html_attr(name = 'style')
## [1] "width:40%;" "width:60%;" "width:60%;" "width:80%;" "width:80%;"

items %>% html_nodes(css = 'dl dd:nth-child(5) div div') %>% html_attr(name = 'style')
## [1] "width:80%;"  "width:100%;" "width:60%;"  "width:100%;" "width:100%;"

items %>% html_nodes(css = 'dl dd:nth-child(7) div div') %>% html_attr(name = 'style')
## [1] "width:40%;" "width:80%;" "width:60%;" "width:60%;" "width:60%;"

items %>% html_nodes(css = 'dl dd:nth-child(9) div div') %>% html_attr(name = 'style')
## [1]  "width:60%;" "width:60%;" "width:60%;" "width:60%;" "width:80%;"

items %>% html_nodes(css = 'dl dd:nth-child(11) div div') %>% html_attr(name = 'style')
## "width:20%;" "width:40%;" "width:60%;" "width:60%;" "width:80%;"

# 개별 기업리뷰 오른쪽에 있는 내용(장점, 단점, 바라는점, 성장예상, 추천여부)을 추출합니다.
items %>% html_nodes(css = 'dl dd:nth-child(2) span') %>% html_text()
##[1] "대기업이라 기업의 조직 체계나 업무 분장, 복리후생제도 등이 타 기업보다는 확실히 정립되어 있음. 월급 새벽 2~3시 칼입금."     
##[2] "자기계발 지원 잘해줌. 영업은 모르겠으나 본사는 연차사용 및 휴직사용 자유로운편"                                             
##[3] "동종업계 1위답게 복지가 매우매우좋다. 급여도 동종업계중 1위로 매우 좋다."                                                   
##[4] "교육프로세스가 잘 갖추어져 있어 지속적으로 업무에 대한 지식 확충이 가능하다. 원천징수에 찍히는 급여지급내역이 엄청나게 많다"
##[5] "그룹 인센티브(흔히 PS) 포함하면 꽤 많은 급여. 네임밸류" 

items %>% html_nodes(css = 'dl dd:nth-child(4) span') %>% html_text()
## [1] "단기 목표 지향적인 문화. 선진 외국 기업의 겉만 따라하려는 문화."                                                                                   
## [2] "영업쪽은 정말 자기 인생이 1도 없다. 야근이 없는 날이 드물며 실적 스트레스도 상당한편"                                                              
## [3] "승진은 역시나 힘들고 매우 잦은 야근과 조직문화가 딱딱함"                                                                                           
## [4] "구내식당 줄이 너무 길어서 밥을 먹을 때 기다렸다가 먹어야 하는 단점이 있습니다.업무 분담이 너무 확실해서 일이 조금 더디게 진행되는 부분이 있습니다."
## [5] "보수적인 분위기, MS 비율 하락되어 시장 경쟁력 확보 필요가 시급합니다."

items %>% html_nodes(css = 'dl dd:nth-child(6) span') %>% html_text()
## [1] "임원분들의 계약기간때문에 장기적인 목표를 가져갈 수 없다는 현실은 이해하지만, 이를 보완할수있는 체계가 마련되어 더 발전했으면" 
## [2] "지점 실적을 줄이거나, 업무강도를 낮춰줘야한다"                                                                                 
## [3] "조직문화를 좀 더 유연하게 만들었으면 좋겠다고 생각함."                                                                         
## [4] "직원들과 대화 나누는 시간이나 직원 가족과 만남을 가져서 대기업이지만 개개인이 아닌 전체 직원이 하나된 마음이 될 수 있었습니다."
## [5] "1위 기업 유지하기 위해 많은 노력과 시장조사가 필요합니다."

items %>% html_nodes(css = 'p.etc_box strong') %>% html_text()
## [1] "비슷" "비슷" "성장" "성장" "비슷"

items %>% html_nodes(css = 'p.txt.recommend.etc_box') %>% html_text()
## [1] "이 기업을 추천 합니다!"       "이 기업을 추천하지 않습니다."
## [3] "이 기업을 추천 합니다!"       "이 기업을 추천 합니다!"      
## [5] "이 기업을 추천 합니다!"


#반복문으로 전체 데이터 수집하기
# 총 페이지 수 계산하고 그 결과를 출력합니다.(한 페이지당 5개의 리뷰) 
pages <- ceiling(x = reviewCnt / 5)
print(x = pages)
## [1] 57

# 결과를 저장할 객체 생성
result <- getData(x = resp)

# result 객체를 출력합니다. 
print(x = result)
str(result)

#전체 리뷰 수집하기
# 반복문을 실행합니다. 
for (page in 2:pages) {
  
  # 작업 시작시각을 저장합니다.
  startTime <- Sys.time()
  
  # 현재 진행사항을 출력합니다.
  cat('[', page, '/', pages, '] 현재 진행 중! ')
  
  # 웹 페이지 URI를 조립합니다.
  cURI <- str_c(URI, '?page=', page)
  
  # HTTP 요청을 합니다.
  resp <- GET(url = cURI, config = list(cookies = myCookies))
  
  # 해당 페이지의 기업리뷰를 추출하여 df 객체에 할당합니다.
  df <- getData(x = resp)
  
  # result 객체에 df 객체를 추가합니다.
  result <- rbind(result, df)
  
  # 작업 종료시각를 저장합니다.
  endTime <- Sys.time()
  
  # 작업에 소요된 시간을 출력합니다.
  (endTime - startTime) %>% print()
  
  # 불필요한 객체를 삭제합니다. 
  rm(resp, df)
  
}

#개별 리뷰코드로 중복여부 확인 
duplicated(x = result$리뷰코드) %>% sum()
## [1] 0

getwd()
setwd('D:/한국경영인증원/R')
# 최종 데이터를 RDS로 저장합니다.
fileNm <- str_c('./data/Company_Review_Data_', compNm, '.RDS')
saveRDS(object = result, file = fileNm)

#Company_Review_Data_삼성화재해상보험.RDS
#Company_Review_Data_카카오.RDS


