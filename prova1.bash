i="minusculo" # declaração não pode ter espaço entre o simbolo de igual

echo $i | tr a-z A-Z

a="MAIUSCULO"
echo $a | tr A-Z a-z

n=5
m=5
for x in `seq 0 2`; do 
    n=$((n+1))
    let m=m-1
    echo $n
    echo $m
done

soma=$((m+n))
echo "A soma eh $soma"

altura=172
if [ $altura -ge 170 ]; then
    echo "Baixo"
else
    echo "Alto"
fi

sentenca="Hoje eh um dia lindo"
for partes in $sentenca
do 
    echo $partes # >> nome.txt (salve em um arquivo txt)
done

dias=("Segunda" "Terca" "Quarta")
echo "Segundo dia: ${dias[1]}"
echo "Todos os dias: ${dias[@]}"

echo "Rian R B P Santana"

vetor=(2 0 2 1 0 3 7 7 8)
sum=0
for n in "${vetor[@]}"; do
    sum=$(( n + sum ))
    echo $sum
done

if (( sum % 2 == 0)); then
    echo "$sum eh par"
else
    echo "$sum eh impar"
fi
    






