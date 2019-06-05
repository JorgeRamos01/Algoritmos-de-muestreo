
#include <Rcpp.h>
using namespace Rcpp;

/* Este programa toma un vector de n�meros positivos (pueden ser indices) y les asigna una probabilidad de muestreo
exceptuando al cero, con respecto al tama�o deseado de la muestra por lo que cumple que la suma de sus entradas debe
 ser igual al tama�o de la muestra que queremos. Este vector per se no permite elegir una muestra pero es usado por
otros algoritmos de muestreo para hacerlo. Lo importante de esto es que cada entrada tiene peso distinto por lo que
el muestreo realizado haciendo uso de este vector de probabilidades de inclusi�n se le conoce como muestreo balanceado.
Este vector de probabilidades de inclusi�n no tiene aleatoriedad en su dise�o por lo que siempre retorna el mismo 
vector cada vez que se hace su c�lculo. Se debe tener cuidado ya que la presencia de valores at�picos afecta su
propiedad de muestreo balanceado por lo que la suma de sus elementos ser� menor que el tama�o de la muestra.*/

// [[Rcpp::export]]
NumericVector inclusionProbs(NumericVector inclProbs,int longSample){
  int control=-1,controlAux=0,i,mayoresUno,longInclProb=inclProbs.size(); //Se definen los controles que nos van a indicar cuando terminar el proceso
  double suma=0;
  NumericVector auxiliar(longInclProb); //Se define un vector auxiliar que nos permitira no reprobabilizar las entradas que se les asigne un peso de 1 o m�s.
  for (i=0;i<longInclProb;i++){     //Rellenamos nuestro vector auxiliar de ceros, en el proceso tomara el valor 1 la entrada que correspondiente a pesos 1 o mayor.
    auxiliar[i]=0;                   //lo que nos permitir� exceptuar dichas entradas para recalcular las probabilidades de inclusi�n para las entradas restantes */   
  }
  while (control!=controlAux){      //Aqu� se inicia el proceso del c�lculo del vector de probabilidades de inclusi�n
    control=controlAux;
    for (i=0; i<longInclProb;i++){
      suma+=inclProbs[i]*(1-auxiliar[i]);  //Se establece la primera versi�n del vector de pesos, no son probabilidades a�n porque puede haber entradas mayores a 1 
    }
    mayoresUno=longSample -controlAux; //Aqui tenemos cuantas entradas de nuestro vector de probabilidades de inclusi�n tienen peso igual a 1
    for (i=0;i<longInclProb;i++){    //Recalculamos las probabilidades de inclusi�n para las entradas que tienen peso menor a 1
      if (auxiliar[i]==0){
        inclProbs[i]=mayoresUno*(inclProbs[i]/suma);
        if (inclProbs[i]>=1.0){      //Si el peso asignado es mayor o igual a uno, se le asigna un uno a esa entrada del vector auxiliar con el fin de no considerarla despu�s para el recalculo de las probabilidades.
          auxiliar[i]=1;              
        }
      }
      else{
        inclProbs[i]=1.0;           //Si la entrada en auxiliar es 1, esto quiere decir que esta entrada en el vector de probabilidades de inclusi�n es uno o mayor, y para que sea una probabilidad le asignamos uno.
      }
    }
  controlAux = sum(auxiliar);     //Calculamos cuantos unos tiene nuestro vector auxiliar, y si no se tuvieron unos nuevos en el vector auxiliar entonces el proceso se termina dado que control al inicio de cada nuevo proceso se le asigna el numero de unos con que se termino el inicial.
  }
  return inclProbs;   //Retornamos nuestro vector de probabilidades de inclusi�n
}

/*Esta es una implementaci�n de la funci�n una vez que ya esta cargada en R, se pide establecer el vector de
 probabilidades de inclusi�n para la esta lista con un tama�o de muestra 3, por lo que la suma del vector
 resultante debe ser 3.
 */

/*** R
inclusionProbs(c(2,3,4,7,5,6,7,9,12),3)
*/
