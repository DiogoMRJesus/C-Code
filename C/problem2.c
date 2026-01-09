#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/**
 * Estrutura para registo de dados sobre cada processo
 * id -> id do processo
 * tempoC -> tempo de chegada
 * prioridade -> prioridade do processo
 * duracao -> duraçao total do processo
 * falta -> duraçao restante do processo (variável que é decrementada)
 * starvation -> variável onde é armazenado o tempo de espera
 * tempoFinalizacao -> regista o momento de finalização do processo
*/
typedef struct{
    int id;
    int tempoC;
    int prioridade;
    int duracao;
    int falta;
    int starvation;
    int tempoFinalizacao;
} Processo;

/*
 * Função que regista os dados dos processos, 
 * introduzidos no standard input  
*/
void registarProcessos(Processo *processos, int n){
    int tmp, prioridade, duracao;

    // repete o ciclo N vezes até ler os dados de todos os processos
    for(int pid = 0; pid < n; pid++){
        scanf("%d %d %d", &tmp, &prioridade, &duracao);
        /**
         * verificação dos limites das variáveis
        */
        if(tmp < 0 || tmp > 100 || prioridade < 1 || prioridade > 100 
            || duracao < 1 || duracao > 10){
            return;
        }else{
            /**
             * Atribuição dos valores às variáveis dos processos 
             * 
            */
            processos[pid].id = pid + 1;
            processos[pid].tempoC = tmp;
            processos[pid].prioridade = prioridade;
            processos[pid].duracao = duracao;
            processos[pid].falta = duracao;
            processos[pid].starvation = 0;
        }
    }
    return;
}

/**
 * n -> número de processos
 * count -> armazena o número de processos já terminados
 * current_time -> armazena o tempo atual (execução do programa)
 * prioridade -> armazena a prioridade do processo escolhido para executar (inicializado a -1 e atualizado no for())
 * index -> armazena a posição do array do processo escolhido para executar (inicializado a -1 e atualizado no for())
 * 
 * TMC -> variável que armazena o tempo médio de circulação
 * 
 * end -> variável boolean que serve de condição para o ciclo while() continuar (caso haja processos por acabar) ou terminar (quando todos os processos terminam)
*/
int main(){
    int n, count = 0, current_time = 0, prioridade, index;
    float TMC = 0;
    bool end = true;

    scanf("%d", &n);
    if(n <= 1 || n >= 100){
        return 0;
    }

    // Alocação de memória para o array que vai armazenar os dados
    Processo *processos = (Processo *) malloc(n * sizeof(Processo)) ;
    // Chamada da função que regista a informação
    registarProcessos(processos, n);

    while(end){
        index = -1;
        prioridade = -1;

        /**
         * escolha do processo a executar segundo o
         * algoritmo de escalonamento por prioridades com preempção
        */
        for(int i = 0; i < n; i++){
            if(processos[i].tempoC <= current_time && processos[i].falta > 0){
                if(prioridade == -1 || processos[i].prioridade < prioridade){  
                    prioridade = processos[i].prioridade;
                    index = i;
                }else if(processos[i].prioridade == prioridade && processos[i].id < processos[index].id){
                    index = i;
                }
            }
        }
 
        /**
         * outputs do programa 
         * IDLE -> nenhum processo está a ser executado
         * else -> informa sobre o processo que está a ser executado e atualiza os dados necessários
        */
        if(index < 0){
            printf("%d-IDLE\n", current_time);
        }else{
            printf("%d-P%d\n", current_time, processos[index].id);
            processos[index].falta--;
            for(int i = 0; i < n; i++){
                if(!(processos[i].id == processos[index].id) && processos[i].tempoC <= current_time && processos[i].falta != 0){ 
                    processos[i].starvation++;  
                }
            }
        }

        processos[index].starvation = 0;

        /**
         * verifica a starvation e caso as condições sejam verdadeiras
         * decremente a prioridade
        */
        for(int i = 0; i < n; i++){
             if(processos[i].starvation == 20  && processos[i].falta != 0){
                if(processos[i].prioridade != 0){
                    processos[i].prioridade--;
                    processos[i].starvation = 0;
                }
            }
        }

        /**
         * verifica se o processo já terminou e caso a condição seja verdadeira
         * atualiza o count é guarda o tempo de término do processo
        */
        if(index != -1 && processos[index].falta == 0){
            processos[index].tempoFinalizacao = current_time;
            count++;
        }

        /**
         * quando todos os processos acabarem o ciclo while termina 
        */
        if(count == n){
            end = false;
        }
        current_time++;
    }
    
    /**
     * calculo do TMC 
    */
    for(int i = 0; i < n; i++){
        TMC = TMC + (processos[i].tempoFinalizacao + 1 - processos[i].tempoC);
    }
    TMC = TMC / n;
    
    printf("TMC=%.1f\n", TMC);

    return 0;
}
