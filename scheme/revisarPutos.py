
with open('scheme/an.txt', 'r') as archivo:
    contenido = archivo.readlines()
    mapa={}
    pun=[]
    cont=0
    for i in contenido:
        x, y=i.split()
        x=x[2: ]
        y = y[: len(y)-1]
        if mapa.get(x, 'noEsta')=='noEsta':
            mapa[x]=[y]
        elif y not in mapa[x]: mapa[x].append(y)
        else: cont+=1
        # if [x, y] in pun: cont+=1
        # else: pun.append([x, y])
    print(cont)
    

