#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h> 
#include <stdlib.h>

#define SZ_NAME 16
#define SZ_BRAND 16

typedef struct{
    int id;
    char name[SZ_NAME];
    int brandId;
    unsigned int weight;
    double price;
} Peca;

#define SZ_PECA (sizeof(Peca))

#define HEADER 28

int NP, NB;

int readFile(int opcao, char *argv[]){
    int fd;

    /**
     * abertura do ficheiro em modo de leitura
     * ou leitura e escrita, consoante a opçao 
    */
    if(opcao == 1 || opcao == 2 || opcao == 5){
        fd = open(argv[1], O_RDONLY);
    }else{
        fd = open(argv[1], O_RDWR);
    }

    return fd;
}

void mostrarHeader(int fd){
    char buffer[20];

    // coloca o ponteiro no inicio do ficheiro
    lseek(fd, 0, SEEK_SET);
    // leitura dos primeiros 28 bytes e armazenamento no buffer 
    read(fd, buffer, 20);
    read(fd, &NP, 4);
    read(fd, &NB, 4);

    printf("HEADER %s %d %d\n", buffer, NP, NB);

    return;
}

void VerPartePeca (int ID, int fd){
    int tam = HEADER + SZ_BRAND * NB + SZ_PECA * ID;
    char brandName[SZ_NAME];

    Peca partePeca;  //cria uma nova peça "partePeca" onde vao ser guardadas as informacoes acerca dessa parte
    if(ID >= NP){
        return;
    }
    lseek(fd, tam, SEEK_SET);
    read(fd, &partePeca, SZ_PECA);

    lseek(fd, (HEADER + SZ_BRAND * partePeca.brandId), SEEK_SET);
    read(fd, brandName, SZ_NAME);

    printf("PECA %d %s %d %s %d %lf\n", partePeca.id, partePeca.name, partePeca.brandId, brandName, partePeca.weight, partePeca.price);

    return;
}

void adicionarPeca(int fd, char *name, int idBrand, unsigned int weight, double price){
    // localização do pointer para mudar NB
    int tam = HEADER - (2 * sizeof(int));

    /**
     * Criação de uma nova peça
     * e atribuição de valores
    */
    Peca novaPeca;
    novaPeca.id = NP++;
    strcpy(novaPeca.name, name);
    novaPeca.brandId = idBrand;
    novaPeca.weight = weight;
    novaPeca.price = price;

    /**
     * colocação do pointer no fim do ficheiro
     * para adicionar a nova peça
    */
    lseek(fd, 0, SEEK_END);
    write(fd, &novaPeca, SZ_PECA);

    // mudança do valor de NB no ficheiro
    lseek(fd, tam, SEEK_SET);
    write(fd, &NP, sizeof(int));

    printf("%d %s %d %d %lf\n", novaPeca.id, novaPeca.name, novaPeca.brandId, novaPeca.weight, novaPeca.price);
    
    return;
}

void VerPartePecaNome (char *name, int fd){
    int tam = HEADER + SZ_BRAND * NB;
    char brandName[SZ_NAME];
    Peca partePeca;  //cria uma nova peça "partePeca" onde vao ser guardadas as informacoes acerca dessa parte
    lseek(fd, HEADER, SEEK_SET);  // dar skip ao primeiros 28 bytes que correspondem ao header


    lseek(fd, tam, SEEK_SET);

    while(read(fd, &partePeca, SZ_PECA) == SZ_PECA){  //verifica se a Peca esta toda preenchida, isto é se o numero de bytes/campos lidos do ficheiro correspondem a estrutura
        if(strcmp(partePeca.name, name) == 0){
            if(partePeca.brandId == 0){
                tam = HEADER;
            }else{
                tam = HEADER + partePeca.brandId * SZ_BRAND;  
            } 
            lseek(fd, tam, SEEK_SET); 
            read(fd, &brandName, SZ_BRAND);
            printf("PECA %d %s %d %s %d %lf\n", partePeca.id, partePeca.name, partePeca.brandId, brandName, partePeca.weight, partePeca.price); //Da print as partes da peca com o ID desejado
            return;
        }
    }
    return;
}

void mudarPeca(int fd, int idPeca, char *nomePeca, int idMarca, unsigned int peso , double preco){
    int tam, tamHB;
    char brandName[SZ_BRAND];
    
    Peca mudarP;
    mudarP.id = idPeca;
    strcpy(mudarP.name, nomePeca);
    mudarP.brandId = idMarca;
    mudarP.weight = peso;
    mudarP.price = preco;

    tamHB = HEADER + mudarP.brandId * SZ_BRAND;  

    lseek(fd, tamHB, SEEK_SET); 
    read(fd, &brandName, 12);
    
    tam = HEADER + NB * SZ_BRAND + SZ_PECA * idPeca;

    lseek(fd, tam, SEEK_SET);
    write(fd, &mudarP, SZ_PECA);

    return;
}

int main(int argc, char *argv[]){
    char nomePeca[SZ_NAME], T;
    int opcao, idPeca, idMarca, peso, fd;
    double preco;

    /**
     * Verifica se o número de argumentos introduzido é valido
    */
    if(argc != 2){
        return 0;
    }

    fd = readFile(1, argv);

    /**
     * verificação de sucesso ou 
     * insucesso na abertura do ficheiro
     * */ 
    if(fd == -1){
        return 0;
    }

    lseek(fd, 20, SEEK_SET);
    read(fd, &NP, 4);
    read(fd, &NB, 4);

    if(NB <= 0 || NB > 1000000 || NP <= 0 || NP > 1000000){
        return 0;
    }

    lseek(fd, 0, SEEK_SET);
    read(fd, &T, 1);
    close(fd);

    /**
     * switch que alterna entre as opções pedidas
    * 1 - mostra o header
    * 2 - localiza a peça na lista pelo id
    * 3 - adiciona uma nova peça à lista
    * 4 - altera os dados da peça (para o ID fornecido)  
    */
    while(opcao != 9){
        scanf("%d", &opcao);

        if((T == '1' && (opcao == 1 || opcao == 5 || opcao == 9))||
            (T == '0' && (opcao == 1 || opcao == 2 || opcao == 3 || opcao == 4 || opcao == 9))){
            
            fd = readFile(opcao, argv);

            switch (opcao){
                case 1: 
                    mostrarHeader(fd);
                    break;
                case 2:
                    scanf("%d", &idPeca);
                    VerPartePeca(idPeca, fd);
                    break;
                case 3:
                    scanf("%s %d %d %lf", &*nomePeca, &idMarca, &peso, &preco);
                    adicionarPeca(fd, nomePeca, idMarca, peso ,preco);
                    break;
                case 4:
                    scanf("%d %s %d %d %lf", &idPeca, &*nomePeca, &idMarca, &peso, &preco);
                    mudarPeca(fd, idPeca, nomePeca, idMarca, peso, preco);
                    break;
                case 5:
                    scanf("%s", &*nomePeca);
                    VerPartePecaNome(nomePeca, fd);
                    break;
                case 9:
                    return 0;
                default:
                    break;
            }
            close(fd);
        }
        while ((getchar()) != '\n');
    }
    return 0;
}
