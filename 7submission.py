import requests
files={'files': open('current_submission.csv','rb')}
data = {
    "user_id": "kohry",
    "team_token": "d9197b1dfffbf863e66775e21648a721ea491e1d53ee2d2e56005df14c44f370",
    "description": 'this is too much',
    "filename": "'current_submission.csv",
}
url = 'https://biendata.com/competition/kdd_2018_submit/'
response = requests.post(url, files=files, data=data)
print(response.text)