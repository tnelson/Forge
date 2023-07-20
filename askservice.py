import sys
import requests

url = "http://localhost:17100/"

# Must be a POST request for the server to process the data properly
if __name__ == '__main__':
    if len(sys.argv) < 3:
        raise IndexError('Usage: askservice <endpoint> <args ...>')
    match sys.argv[1]:
        case 'load':
            f = open(sys.argv[2], "r")
            filedata: str = f.read()
            r = requests.post(url = url+"load", data = filedata)
            print(r.json())

        case 'sat':
            params = {'id': sys.argv[2]}
            r = requests.get(url = url+"sat", params = params)
            print(r.json())

        case 'next':
            params = {'id': sys.argv[2]}
            r = requests.get(url = url+"next", params = params)
            print(r.json())

