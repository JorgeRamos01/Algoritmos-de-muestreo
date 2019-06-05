#include <Rcpp.h>
#include <stdlib.h>
#include <time.h>
using namespace Rcpp;

/* Este es un algoritmo de muestreo aleatorio simple sin reemplazo cuya mayor virtud es que puede usarse 
 con vectores muy grandes y con una ligera modificaci�n cambiando el �ltimo for por un while y una condici�n
 apropiada puede usarse con vectores exageradamente grandes que podemos no conocer su tama�o. Se le conoce como
 algoritmo de muestreo de reserva y la idea de su uso es muy simple: Se genera un vector del tama�o deseado de la
 muestra que denotaremos por n y se almacenan los primeros n valores de nuestro vector de poblaci�n y despu�s las
 dem�s entradas se van a elegir para reemplazar alguna de las n entradas de nuestro vector de muestra con una
 probabilidad de 1 entre el indice de la entrada en la que estemos parados en nuestro vector de poblaci�n, es decir
 elegimos de manera uniforme discreta, con la misma probabilidad se elige el elemento de la muestra a ser reemplazado. */

// [[Rcpp::export]]
NumericVector reservoirSampling(NumericVector poblacion, int longMuestra) {
  srand(time(NULL));                //Hacemos variaciones de la semilla para aumentar la aleatoriedad del algoritmo
  NumericVector muestra(longMuestra);
  int i,indiceAuxiliar;
  for (i=0; i<longMuestra; i++){    //Guardamos los primeras n entradas del vector de poblaci�n en nuestro vector muestra
    muestra[i]=poblacion[i];
  }
  for (i=longMuestra; i<poblacion.size();i++){
    indiceAuxiliar=rand()%i;     //Establecemos el posible indice de intercambio
    if(indiceAuxiliar<longMuestra){    //Si dicho posible indice de intercambio es menor que es menor que el tama�o de
                                        // la muestra entontes hacemos el intercambio.
      muestra[indiceAuxiliar]=poblacion[i];
    }
  }
  return muestra;
}

/*Este es un ejemplo al tener la funci�n precargada en R, en el que elegimos 3 elementos aleatoriamente de
 la poblaci�n para armar nuestra muestra.  */

/*** R
reservoirSampling(c(1,2,6,3,5,4,89,5,2,4,3),3)
*/
