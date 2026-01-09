#include <stdio.h>
#include <stdbool.h>

#define TAM 100

typedef struct{
    int montra[4];
    int vistos[4];
    int A;
    int ind;
}Carros;

int count;

// gerador de número aleatório int {0,1,2,3}
int myrand () {
    static int valor = 45;
    
    valor = 73 * valor + 2713;
    valor = (valor % 101);

    return (valor%4);
}

/*
* verifica se o carro que o cliente quer ver está na montra
* count = 4 -> nenhum dos carros em montra é o que o cliente quer ver
*/
int verificarMontra(Carros carros, int opcaoAtual){
    count = 0;

    for(int i = 0; i < 4; i++){
        if(carros.montra[i] != opcaoAtual){
            count++;
        }
    }

    return count;
}

/*
* algoritmo de LRU
* Least Recently Used 
*/
int LRU(Carros carros, int *car, int N) {
    int n1 = 0; 

    for (int i = 0; i < N; i++) {
        int found = -1; 

        // Verifica se o carro atual está na montra
        for (int j = 0; j < 4; j++) {
            if (carros.montra[j] == car[i]) {
                found = j;
                break;
            }
        }

        // caso: "carro não encontrado"
        if (found == -1) { 
            int indAntigo = 0; 
            // encontra o carro visto à mais tempo pelo cliente
            for (int j = 1; j < 4; j++) {
                if (carros.vistos[j] < carros.vistos[indAntigo]) {
                    indAntigo = j;
                }
            }
            // troca o carro visto à mais tempo pelo atual não presente na montra
            carros.montra[indAntigo] = car[i];
            carros.vistos[indAntigo] = i; 

            n1++;
        // caso: "carro encontrado" 
        } else {
            carros.vistos[found] = i;
        }
    }

    return n1;
}


/*
* função que troca um dos carros em montra pelo 
* escolhido para ver pelo cliente
*/ 
int random(Carros carros, int *car, int N){
    int rand, n2 = 0;

    for(int i = 0; i < N; i++){
        count = verificarMontra(carros,car[i]);
        if(count == 4){
            rand = myrand();
            carros.montra[rand] = car[i];
            n2++;
        }
    }
    return n2;
}

/*
* funçãoque troca um dos carros em montra pelo
* primeiro a ter sido colocado em montra 
*/
int FIFO(Carros carros, int *car, int N){
    int n3 = 0;

    for(int i = 0; i < N; i++){
        count = verificarMontra(carros, car[i]);
        if(count == 4){
            if(carros.ind == 4){
                carros.ind= 0;
            }
            carros.montra[carros.ind] = car[i];
            carros.ind++;

            n3++;
        }
    }
    return n3;
}

/*
* função que troca os carros de forma ideal de acordo 
* com o que o cliente vai querer ver
*/
int ideal(Carros carros, int *car, int N) {
    int n4 = 0;

    for (int i = 0; i < N; i++) {
        count = verificarMontra(carros, car[i]);

        if (count == 4) {
            int IndexMaisLonge = -1, substIndex = -1;

            // Encontra o carro na montra que será usado mais tarde ou nunca
            for (int j = 0; j < 4; j++) {
                int prox = -1;
                // Procura quando o carro na posição j será necessário novamente
                for (int k = i + 1; k < N; k++) {
                    if (car[k] == carros.montra[j]) {
                        prox = k;
                        break;
                    }
                }

                // Se o carro na posição j não é mais necessário
                if (prox == -1) {
                    substIndex = j; // Marca este índice para substituição
                    break;
                // Atualiza o índice que será necessário mais tarde
                } else if (prox > IndexMaisLonge) {
                    IndexMaisLonge = prox;
                    substIndex = j;
                }
            }

            carros.montra[substIndex] = car[i];
            
            n4++; 
        }
    }
    return n4;
}

int main(){
    /*
    * n1 -> nº de trocas no algoritmo LRU
    * n2 -> nº de trocas no algoritmo random
    * n3 -> nº de trocas no algoritmo FIFO
    * n4 -> nº de trocas ideal 
    */
    int N, n1 = 0, n2 = 0, n3 = 0, n4 = 0;
    int carros[TAM];

    // lê os inputs por ordem dos modelos que o cliente quer ver
    scanf("%d", &N);

    if(N > 100){
        return 0;
    }

    // regista a ordem que a pessoa quer ver os carros
    for(int i = 0; i < N; i++){
        scanf("%d", &carros[i]);
    }

    /* 
    * criação da estrutura que armazena os dados necessários 
    * para a execução dos algoritmos
    */
    Carros carrosMontra;
    carrosMontra.ind = 0;

    // inicia a montra do stand com os carros do modelo 1 até 4
    for(int i = 0; i < 4; i++){
        carrosMontra.montra[i] = i + 1;
    }

    // algoritmo LRU
    n1 = LRU(carrosMontra, carros, N);

    // algoritmo RANDOM
    n2 = random(carrosMontra, carros, N);

    // algoritmo FIFO
    n3 = FIFO(carrosMontra, carros, N);

    // algoritmo IDEAL
    n4 = ideal(carrosMontra, carros, N);

    printf("LRU : %d\n", n1);
    printf("Rand : %d\n", n2);
    printf("FIFO : %d\n", n3);
    printf("IDEAL : %d\n", n4);
}
