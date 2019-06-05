#include <Rcpp.h>
using namespace Rcpp;

 /*Este programa auxiliar tomar un numero de tipo doble y lo transforma en un entero. retorno uno si el doble es mayor
 o igual a 1/2, y cero en caso contrario */

 int asInt(double x){
  int entero;
  if (x>=0.5){
    entero=1;
  }
  else{
    entero=0;
  }
  return entero;
}

 /*Este programa es el mismo que el RandomSort y sirve para muestrear un subconjunto del vector poblacion y ordenarlo
  de una manera aleatoria 
  */
NumericVector randomSort(NumericVector x,int n) {
  int longitudVector=x.size(),indice, ind_aux;
  NumericVector auxiliar=runif(longitudVector);
  NumericVector muestra(n);
  for(indice=0;indice<longitudVector;indice++){
    for (ind_aux=0;ind_aux<longitudVector-indice-1;ind_aux++){
      if (auxiliar[ind_aux]<auxiliar[ind_aux+1]){
        auxiliar[ind_aux]=auxiliar[ind_aux+1]+auxiliar[ind_aux];
        x[ind_aux]=x[ind_aux+1]+x[ind_aux];
        auxiliar[ind_aux+1]=auxiliar[ind_aux]-auxiliar[ind_aux+1];
        x[ind_aux+1]=x[ind_aux]-x[ind_aux+1];
        auxiliar[ind_aux]=auxiliar[ind_aux]-auxiliar[ind_aux+1];
        x[ind_aux]=x[ind_aux]-x[ind_aux+1];
      }
    }
  }
  for (indice=0;indice<n;indice++){
    muestra[indice]=x[indice];
  }
  return muestra;
}

/*Este programa recibe un vector de probabilidades de inclusión y retorna un vector de unos y ceros del tamaño del vector
 de probabilidades de inclusión que es el mismo tamaño del vector población, con tantos unos como el tamaño de la muestra 
sea y que viene implícita en las propiedades del vector de probabilidades de inclusión. Este es un algoritmo conocido 
como el método del pivote y es un diseño de muestreo sin reemplazo. Es una variación del método del pivote ya que reordena
 al principio el vector de probabilidades de inclusión haciendo que la medida de entropia se maximize. Consiste en partir 
 el vector de probabilidades de inclusión en dos partes, y generando un nuevo vector de probabilidades de inclusión a partir 
 de las 2 partes anteriores, y se vuelve a partir en 2 y con ello validando condiciones para llenar nuestro vector de unos 
 y ceros resultante. Debido a que el proceso solo depende del vector de probabilidades de inclusión inmediato anterior se 
 le denota como un algoritmo martingala. En particular este método entre los que parten el vector de probabilidades de 
 inclusión en cada paso solo modifica dos entradas del vector anterior de probabilidades de inclusión.
*/

// [[Rcpp::export]]
NumericVector metodoPivote(NumericVector inclProb) {
  int longInclProb=inclProb.size(),i;
  NumericVector muestra(longInclProb);
  randomSort(inclProb,longInclProb);
  double tolerancia=0.00001,split1=inclProb[0],split2=inclProb[1],randUnif; //Incluí una tolerancia porque el proceso puede llevar mucho tiempo hasta convertirse en un entero (0 o 1)
  int control1=0,control2=1, control3=2; //Definimos las variables que nos van a establecer el control del orden del pivoteo
  for (i=0;i<longInclProb;i++){
    muestra[i]=0.0;
  }
  while(control3<=longInclProb){
    randUnif=runif(1)[0]; //Generamos un numero aleatorio uniforme(0,1) para comparar
    if(split1>=tolerancia && split1<= 1-tolerancia && split2>=tolerancia && split2<= 1-tolerancia){ //Establecemos las condiciones de tolerancia
      if(split1+split2>1.0){     //Revisamos las condiciones propias del algoritmo para recalcular el vector de probabilidades de inclusión
        if(randUnif<(1-split2)/(2-split1-split2)){
          split2=split1+split2-1;
          split1=1.0;
        }
        else {
          split1=split1+split2-1;
          split2=1.0;
        }
      }
      else{
        if(randUnif< split2/(split1+split2)){ 
          split2= split1+split2;
          split1=0.0;
        } 
        else{
          split1= split1+split2;
          split2=0.0;
        } 
      }
      if( ((split1<tolerancia) | (split1 > 1-tolerancia)) && (control3<=longInclProb)){ //En los siguiente 2 if's revisamos al cumplirse cada cambio del vector de probabilidades de inclusión para ver si continuamos ejecutando el primer proceso o se termina
        muestra[control1]=asInt(split1);
        split1=inclProb[control3];
        control1=control3;
        control3=control3+1;
      } 
      if( ((split2<tolerancia) | (split2 > 1-tolerancia)) && (control3<=longInclProb)){ 
        muestra[control2]=asInt(split2);
        split2=inclProb[control3];
        control2=control3;
        control3=control3+1;
      } 
    }
  }
  randUnif=runif(1)[0]; //Volvemos a generar un número aleatorio uniforme(0,1)
  if(split1>=(tolerancia) && (split1)<= (1-tolerancia) && (split2>=tolerancia) && (split2<= 1-tolerancia)){ //revisamos las condiciones de tolerancia
    if(split1+split2>1){       // y revisamos las condiciones de asignación dentro del vector de probabilidades de inclusión
      if(randUnif<(1-split2)/(2-split1-split2)){
        split2=split1+split2-1;
        split1=1.0;
      } 
      else{
        split1=split1+split2-1;
        split2=1.0;
      } 
    }
    else{
      if(randUnif< split2/(split1+split2)){ 
        split2=split1+split2;
        split1=0.0;
      } 
      else{
        split1= split1+split2;
        split2=0.0;
      } 
    }
  }
  muestra[control1]=asInt(split1); //Agregamos los últimos elementos del vector de ceros y unos.
  muestra[control2]=asInt(split2);
  return muestra;            //Regresamos el vector de ceros y unos, con tantos ceros como nos lo indique nuestro vector de probabilidades de inclusión
}

/*Este es un pequeño ejemplo del programa una vez que esta cargado en R. El vector de probabilidades de inclusión
 usado es el resultado del ejemplo del vector de probabilidades de inclusión generado en el programa específico 
 para eso, por lo que nos debe de dar solo 3 unos en el vector no necesariamente en las mismas posiciones cada 
 vez que llamemos la función con este vector de probabilidades de inclusión en específico.
*/

/*** R
metodoPivote(c(0.1090909,0.1636364,0.2181818, 0.3818182, 0.2727273, 0.3272727, 0.3818182, 0.4909091, 0.6545455))
  */
