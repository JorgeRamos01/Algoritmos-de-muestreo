#include <Rcpp.h>
using namespace Rcpp;

/*Este algoritmo de muestre es conocido como de selecci�n - rechazo, y es considerado el mejor algoritmo
para elegir una muestra de acuerdo el muestreo aleatorio simple sin reemplazo, aparte de ser un algoritmo
 completamente secuencial. Aunque tiene la limitaci�n que no siempre llega a elegir por completo los elementos
 necesarios para completar la muestra por lo que en esta versi�n se incluye la condici�n de repetir el proceso
 hasta que se complete el vector con el tama�o de la muestra deseado, lo cual limita la velocidad final, pero 
  la aleatoriedad establecida por la elecci�n de valores aleatorios uniformes en (0,1) permite que el algoritmo
  tenga una complejidad lineal O(n).*/

// [[Rcpp::export]]
NumericVector selectReject(NumericVector poblacion,int longMuestra) {
  NumericVector muestra(longMuestra);
  int i,longPoblacion=poblacion.size();
  double aceptados=0.0;
  while (TRUE){
    for (i=0; i<longPoblacion;i++){
      /*Condici�n de aceptaci�n en el vector muestra que establece que si un cociente particular es mayor 
       que un numero aleatorio generado por una uniforme(0,1) entonces se acepta dentro del vector muestra */
      if ((longMuestra-aceptados)/(longPoblacion-i+1)>runif(1)[0]){  
          aceptados +=1.0;       //en caso de ser aceptado se incrementa en uno esta variable
          muestra[(int)aceptados -1]=poblacion[i];  //Se agrega dicha entra del vector poblaci�n al vector de la muestra  
      }
    }
    if(aceptados==(double)longMuestra){ //Condici�n de quiebre en caso llenar el vector muestra
      break;
    }
    else{
      aceptados=0.0;  //En caso de no completarse la muestra reiniciamos el proceso de selecci�n.
    }
  }
  return muestra;
}

/*Este es un ejemplo de su uso en R una vez cargada la funci�n. En donde elegiremos 5 elementos para la 
  establecer nuestra muestra.
 */

/*** R
selectReject(c(1,5,4,6,3,8,5,7,65,789,42,6,36,45,21,56,353,7483,1235,1235,124,87),10)
*/
