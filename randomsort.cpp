#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

/*Este programa es un algoritmo de muestreo aleatorio simple sin reemplazo. Requiere 2 entradas: la población
 de la cual se va a hacer el muestreo en forma de vector de R, y un entero que denota el tamaño deseado de la
 muestra. Es un algoritmo simple en el que se lee la población y se genera un vector aleatorio del mismo tamaño
 que la población con entradas números aleatorios generados por una distribución uniforme(0,1), el cual se toma
 como referencia para ordenar el vector de la población de manera decreciente. El algoritmo usa Bubble Sort para
 hacer el ordenamiento ya que hacer un ordenamiento simultaneo con algún algoritmo que utilice recursividad puede
 complicar el algoritmo.
 */ 

// [[Rcpp::export]]
NumericVector RandomSort(NumericVector x,int n) {
  int longitudVector=x.size(),indice, ind_aux;
  NumericVector auxiliar=runif(longitudVector);
  NumericVector muestra(n);                       //Este es nuestro vector que nos dara la muestra de la poblacion
  /*Aqui implementamos un algoritmo BubbleSort para ordenar el vector con valores uniformes usando los índices 
   para ordenar simultaneamente el vector de la población. */
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
  /*Elegimos los n valores que corresponder a los indices de los n valores más grandes que generamos en 
  nuestro vector de variables uniformes(0,1)*/
  for (indice=0;indice<n;indice++){   
    muestra[indice]=x[indice];
  }
  return muestra;  // El resultado es un vector del tamaño que elegimos para la muestra.
}


/*Este es un ejemplo de su uso donde le damos un vector de 6 elementos y pedimos una muestra aleatoria de
 3 elementos. Al realizarlo varias veces el vector resultante debe ir variando sin ningún patrón particular
para que se note que realmente hay aleatoriedad en la selección.*/

/*** R
RandomSort(c(1,2,3,6,8,9),3)   
*/
