//f.prototype = prototipos de las instancias creadas a partir de f
//getProtypeOf(f) = el prototipo de f, function.prototype

AgenteDeControl = function() {
    this.agencia = "Control"
};

smart = new AgenteDeControl();

Agencia = function( programa, idFuncion, numeroTotalFuncion) {
    this.programaDeEntrenamiento = programa;
    this.idFuncion = idFuncion;
    this.numeroTotalFuncion = numeroTotalFuncion;
    this.programaDeEntrenamiento.prototype[numeroTotalFuncion] = 0;

    this.programaDeEntrenamiento.prototype.espiar = function(agenciaAEspiar) {
        Object.setPrototypeOf(this, agenciaAEspiar.programaDeEntrenamiento.prototype);
    }

    this.incrementar = function(){
    	this.programaDeEntrenamiento.prototype[numeroTotalFuncion]++;
    };
};

//control = new Agencia(AgenteDeControl);
control = new Agencia(AgenteDeControl, "idC", "nC");

// no me gusta pasarle el agente como parametro
realizarProgramaDeEntrenamiento = function(agente, agencia) {
    completarProgramaDeEntrenamiento = agencia.programaDeEntrenamiento.bind(agente); // bind reemplaza al this, que es nuevo, por este agente en particular
    completarProgramaDeEntrenamiento();
};

asignarIdAAgente = function(agente, agencia) {
    agente[agencia.idFuncion] = agente[agencia.numeroTotalFuncion];
    // Actualizamos la cantidad de agentes en la agencia
    agencia.incrementar();
}

registrarAgente = function(agente, agencia) {
    asignarIdAAgente(agente, agencia);
    agente.dejarDeEspiar = function() {
        Object.setPrototypeOf(agente, agencia.programaDeEntrenamiento.prototype);
    };
}

nuevoAgente = function(agencia) {
    let agente = new agencia.programaDeEntrenamiento();
    registrarAgente(agente, agencia);
    return agente;
};

enrolar = function(agente, agencia){
    completarProgramaDeEntrenamiento = agencia.programaDeEntrenamiento.bind(agente); // bind reemplaza al this, que es nuevo, por este agente en particular
    completarProgramaDeEntrenamiento();
    // Con bind
    // se crea un obj, se ejecuta la func con el this, como este nuevo obj,
    //y al objeto nuevo le pone como prototype el prototypo de las instancias de esa funcion

    Object.setPrototypeOf(agente, agencia.programaDeEntrenamiento.prototype);
    registrarAgente(agente, agencia);
};

agenteEspecial = function(agencia, habilidad){
    let agente = nuevoAgente(agencia);
    agente[habilidad.name] = habilidad;
    return agente;
};

camuflar = function(objetoACamuflar) {
    // Asumimos que los mensajes de objetoACamuflar son distintos que los de this (Consultado)
    Object.assign(this, objetoACamuflar);
};

// Ejemplo de un test
function testEjemplo(res) {
  res.write("\n|| Probando la suma ||\n");
  let sumando1 = 4;
  let sumando2 = 6;
  let resultado_obtenido = sumando1 + sumando2;
  let resultado_esperado = 10;
  res.write("El resultado de sumar " + sumando1 + " y " + sumando2 + " da " + resultado_obtenido, (resultado_obtenido===resultado_esperado));
  sumando1 = "4";
  sumando2 = "6";
  resultado_obtenido = sumando1 + sumando2;
  resultado_esperado = "10";
  res.write("El resultado de sumar " + sumando1 + " y " + sumando2 + " da " + resultado_obtenido, (resultado_obtenido===resultado_esperado));
  sumando1 = 4;
  sumando2 = undefined;
  resultado_obtenido = sumando1 + sumando2;
  res.write("El resultado de sumar " + sumando1 + " y " + sumando2 + " da " + resultado_obtenido);
}

// Test Ejercicio 1
function testEjercicio1(res) {
  res.write("\n|| Crear al agente Smart ||\n");
  let creadoCorrectamente = Object.getPrototypeOf(smart) === AgenteDeControl.prototype;
  res.write(`Agente Smart creado de forma ${creadoCorrectamente ? '' : 'in'}correcta`, creadoCorrectamente);
  let conoceSuAgencia = "agencia" in smart;
  res.write(`Agente Smart ${si_o_no(conoceSuAgencia)} conoce su agencia`, conoceSuAgencia);
  let suAgenciaEsControl = smart.agencia === "Control";
  res.write(`La Agencia del agente Smart ${si_o_no(suAgenciaEsControl)} es Control`, suAgenciaEsControl);
  // Completar

}

// Test Ejercicio 2
function testEjercicio2(res) {
  let agenciaEstaDefinida = Agencia != undefined;
  res.write(`La función Agencia ${si_o_no(agenciaEstaDefinida)} está definida`, agenciaEstaDefinida);

  let AgenteDeKaos = function() {};
  kaos = new Agencia(AgenteDeKaos);
  let tieneDefinidoElProgramaDeEntrenamiento = Object.values(kaos).includes(AgenteDeKaos);
  res.write(`La agencia Kaos ${si_o_no(tieneDefinidoElProgramaDeEntrenamiento)} tiene definido un programa de entrenamiento`, tieneDefinidoElProgramaDeEntrenamiento);
  // Completar
  let tieneDefinidoElProgramaDeEntrenamientoControl = Object.values(control).includes(AgenteDeControl);
  res.write(`La agencia Control ${si_o_no(tieneDefinidoElProgramaDeEntrenamientoControl)} tiene definido un programa de entrenamiento`, tieneDefinidoElProgramaDeEntrenamiento);

}

// Test Ejercicio 3
function testEjercicio3(res) {
  res.write("\n|| Crear una agencia y un agente ||\n");
  let fConstructora = function() { };
  fConstructora.prototype.peliculas = 2;
  let oss = new Agencia(fConstructora);
  let miniEspia = nuevoAgente(oss);
  let conoceMensajePrototipo = miniEspia.peliculas === 2;
  res.write(`El agente ${si_o_no(conoceMensajePrototipo)} conoce el mensaje de su prototipo`, conoceMensajePrototipo);
  fConstructora.prototype.peliculas = 3;
  let mensajePrototipoActualizado = miniEspia.peliculas === 3;
  res.write(`El agente ${si_o_no(mensajePrototipoActualizado)} sabe que el mensaje se actualizó`, mensajePrototipoActualizado);
  res.write("\n|| Enrolar a un agente ||\n");
  let miniEspia2 = {};
  enrolar(miniEspia2, oss);
  conoceMensajePrototipo = miniEspia2.peliculas === 3;
  res.write(`El agente enrolado ${si_o_no(conoceMensajePrototipo)} conoce el mensaje de su prototipo`, conoceMensajePrototipo);
  fConstructora.prototype.peliculas = 1;
  mensajePrototipoActualizado = miniEspia2.peliculas === 1;
  res.write(`El agente enrolado ${si_o_no(mensajePrototipoActualizado)} sabe que el mensaje se actualizó`, mensajePrototipoActualizado);

  let agentes = 0;
  let fConstructora2 = function() {
    agentes++;
  };
  let cia = new Agencia(fConstructora2);
  let miniEspia3 = {};
  enrolar(miniEspia3, cia);
  agenciaRegistraEnrolamiento = agentes == 1;
  res.write(`El agente enrolado ${si_o_no(agenciaRegistraEnrolamiento)} pasó por el programa de entrenamiento`, agenciaRegistraEnrolamiento);
  // Completar

}

// Test Ejercicio 4
function testEjercicio4(res) {
  res.write("\n|| Crear un agente de cada agencia ||\n");
  control = new Agencia(function() { }, "idC", "nC");
  kaos = new Agencia(function() { }, "idK", "nK");
  let agenteK = {};
  let agenteC = nuevoAgente(control);
  enrolar(agenteK, kaos);

  let C_conoce_idC = "idC" in agenteC;
  let C_conoce_nC = "nC" in agenteC;
  let C_conoce_idK = "idK" in agenteC;
  let C_conoce_nK = "nK" in agenteC;
  let K_conoce_idC = "idC" in agenteK;
  let K_conoce_nC = "nC" in agenteK;
  let K_conoce_idK = "idK" in agenteK;
  let K_conoce_nK = "nK" in agenteK;
  res.write("El agente de Control" + si_o_no(C_conoce_idC) + "sabe responder idC", C_conoce_idC);
  res.write("El agente de Control" + si_o_no(C_conoce_nC) + "sabe responder nC", C_conoce_nC);
  res.write("El agente de Control" + si_o_no(C_conoce_idK) + "sabe responder idK", !C_conoce_idK);
  res.write("El agente de Control" + si_o_no(C_conoce_nK) + "sabe responder nK", !C_conoce_nK);
  res.write("El agente de Kaos" + si_o_no(K_conoce_idC) + "sabe responder idC", !K_conoce_idC);
  res.write("El agente de Kaos" + si_o_no(K_conoce_nC) + "sabe responder nC", !K_conoce_nC);
  res.write("El agente de Kaos" + si_o_no(K_conoce_idK) + "sabe responder idK", K_conoce_idK);
  res.write("El agente de Kaos" + si_o_no(K_conoce_nK) + "sabe responder nK", K_conoce_nK);

  // El agente tiene que saber el numero de agentes en su agencia
  res.write("\n|| Agentes conocen numero de agentes en su agencia ||\n");
  let numControl = agenteC.nC;
  let agenteC_conoce_nC = numControl == 1;
  let numKaos = agenteK.nK;
  let agenteK_conoce_nK = numKaos == 1;
  res.write("El agente de Control" + si_o_no(agenteC_conoce_nC) + "sabe correctamente nC", agenteC_conoce_nC);
  res.write("El agente de Kaos" + si_o_no(agenteK_conoce_nK) + "sabe correctamente nK", agenteK_conoce_nK);

  // Verificamos que los agentes tengan id's crecientes(empiezan desde 0)
  res.write("\n|| Los numeros de los agentes estan en orden creciente ||\n");
  let agC2 = nuevoAgente(control);
  let agC3 = {};
  enrolar(agC3, control);
  let agK2 = nuevoAgente(kaos);
  let agK3 = {};
  enrolar(agK3, kaos);

  res.write("El agenteC de Control tiene id: 0" + si_o_no(agenteC.idC == 0), agenteC.idC == 0);
  res.write("El agente 2 de Control tiene id: 1" + si_o_no(agC2.idC == 1), agC2.idC == 1);
  res.write("El agente 3 de Control tiene id: 2" + si_o_no(agC3.idC == 2), agC3.idC == 2);

  // Verificamos que los agentes tengan el numero de agentes que trabajan en su misma agencia actualizado
  res.write("\n|| Los agentes tienen el numero de agentes en su agencia actualizado ||\n");
  numControl = agenteC.nC;
  agenteC_conoce_nC = numControl == 3;
  numKaos = agenteK.nK;
  agenteK_conoce_nK = numKaos == 3;
  res.write("El agente de Control" + si_o_no(agenteC_conoce_nC) + "actualiza nC", agenteC_conoce_nC);
  res.write("El agente de Kaos" + si_o_no(agenteK_conoce_nK) + "actualiza nK", agenteK_conoce_nK);
  res.write("Numero de agentes en Control: " + agenteC.nC);
  res.write("Numero de agentes en Kaos: " + agenteK.nK);
}

// Test Ejercicio 5
function testEjercicio5(res) {
  res.write("\n|| Crear un agente de cada agencia y mandarlo a espiar ||\n");
	control = new Agencia(function() { }, "idC", "nC");
	kaos = new Agencia(function() { }, "idK", "nK");
	let agenteK = nuevoAgente(kaos);
  let agenteC = {};
  enrolar(agenteC, control);
  agenteC.espiar(kaos);
  agenteK.espiar(control);
  let C_conoce_idC = "idC" in agenteC;
  let C_conoce_nC = "nC" in agenteC;
  let C_conoce_idK = "idK" in agenteC;
  let C_conoce_nK = "nK" in agenteC;
  let K_conoce_idC = "idC" in agenteK;
  let K_conoce_nC = "nC" in agenteK;
  let K_conoce_idK = "idK" in agenteK;
  let K_conoce_nK = "nK" in agenteK;
  res.write("El espía de Control" + si_o_no(C_conoce_idC) + "sabe responder idC", C_conoce_idC);
  res.write("El espía de Control" + si_o_no(C_conoce_nC) + "sabe responder nC", !C_conoce_nC);
  res.write("El espía de Control" + si_o_no(C_conoce_idK) + "sabe responder idK", !C_conoce_idK);
  res.write("El espía de Control" + si_o_no(C_conoce_nK) + "sabe responder nK", C_conoce_nK);
  res.write("El espía de Kaos" + si_o_no(K_conoce_idC) + "sabe responder idC", !K_conoce_idC);
  res.write("El espía de Kaos" + si_o_no(K_conoce_nC) + "sabe responder nC", K_conoce_nC);
  res.write("El espía de Kaos" + si_o_no(K_conoce_idK) + "sabe responder idK", K_conoce_idK);
  res.write("El espía de Kaos" + si_o_no(K_conoce_nK) + "sabe responder nK", !K_conoce_nK);
  // Completar

  // agenteC tiene que poder ir a espiar a su agencia original
  res.write("\n|| Crear un doble agente  ||\n");
  agenteC.espiar(control)
  C_conoce_idC = "idC" in agenteC;
  C_conoce_nC = "nC" in agenteC;
  C_conoce_idK = "idK" in agenteC;
  C_conoce_nK = "nK" in agenteC;
  res.write("El espía de Control" + si_o_no(C_conoce_idC) + "sabe responder idC", C_conoce_idC);
  res.write("El espía de Control" + si_o_no(C_conoce_nC) + "sabe responder nC", C_conoce_nC);
  res.write("El espía de Control" + si_o_no(C_conoce_idK) + "sabe responder idK", !C_conoce_idK);
  res.write("El espía de Control" + si_o_no(C_conoce_nK) + "sabe responder nK", !C_conoce_nK);

  // Verificamos que el agente Kaos si deja de espiar pueda responder los mensajes de Kaos
  res.write("\n|| Dejar de espiar con el agente de Kaos ||\n");
  agenteK.dejarDeEspiar();
  K_conoce_idC = "idC" in agenteK;
  K_conoce_nC = "nC" in agenteK;
  K_conoce_idK = "idK" in agenteK;
  K_conoce_nK = "nK" in agenteK;
  res.write("El espía de Kaos" + si_o_no(K_conoce_idC) + "sabe responder idC", !K_conoce_idC);
  res.write("El espía de Kaos" + si_o_no(K_conoce_nC) + "sabe responder nC", !K_conoce_nC);
  res.write("El espía de Kaos" + si_o_no(K_conoce_idK) + "sabe responder idK", K_conoce_idK);
  res.write("El espía de Kaos" + si_o_no(K_conoce_nK) + "sabe responder nK", K_conoce_nK);

  res.write("\n|| Dejar de espiar con el agente doble ||\n");
  agenteC.dejarDeEspiar();
  C_conoce_idC = "idC" in agenteC;
  C_conoce_nC = "nC" in agenteC;
  C_conoce_idK = "idK" in agenteC;
  C_conoce_nK = "nK" in agenteC;
  res.write("El espía de Control" + si_o_no(C_conoce_idC) + "sabe responder idC", C_conoce_idC);
  res.write("El espía de Control" + si_o_no(C_conoce_nC) + "sabe responder nC", C_conoce_nC);
  res.write("El espía de Control" + si_o_no(C_conoce_idK) + "sabe responder idK", !C_conoce_idK);
  res.write("El espía de Control" + si_o_no(C_conoce_nK) + "sabe responder nK", !C_conoce_nK);


}

// Test Ejercicio 6
function testEjercicio6(res) {
  let fConstructora = function() {
    this.sombrero = true;
  };
  // agencia con agentes con sombrero
  let owca = new Agencia(fConstructora);
  let sacarseElSombrero = function() {
    this.sombrero = false;
  }
  res.write("\n|| Crear al agente P ||\n");
  let p = new agenteEspecial(owca, sacarseElSombrero);
  let perrySabeSacarseElSombrero = 'sacarseElSombrero' in p;
  res.write(`El agente P ${si_o_no(perrySabeSacarseElSombrero)} sabe sacarse el sombrero`, perrySabeSacarseElSombrero);

  let tieneElSombreroPuesto = p.sombrero;
  res.write(`Tiene el sombrero puesto ${si_o_no(tieneElSombreroPuesto)}`, tieneElSombreroPuesto);
  res.write("H" + tieneElSombreroPuesto);
    p.sacarseElSombrero();
  let seSacoElSombrero = !p.sombrero;
  res.write(`Se saco el sombrero ${si_o_no(seSacoElSombrero)}`, seSacoElSombrero);

  res.write("\n|| Crear al agente Camaleón ||\n");
  let camaleon = new agenteEspecial(owca, camuflar);
  // llamamos a camuflar con parametro
  camaleon.camuflar({
    color: 'rojo',
    repetir: function(s) {return s;}
  });

  let camaleonSabeResponderColor = 'color' in camaleon;
  let camaleonSabeResponderRepetir = 'repetir' in camaleon;
  res.write(`El agente Camaleón ${si_o_no(camaleonSabeResponderColor)} sabe responder \"color\"`, camaleonSabeResponderColor);
  res.write(`El agente Camaleón ${si_o_no(camaleonSabeResponderRepetir)} sabe responder \"repetir\"`, camaleonSabeResponderRepetir);

  let camaleonEsRojo = camaleon.color == 'rojo';
  let camaleonRespondeQOnda = camaleon.repetir('q onda?') == 'q onda?';
  res.write(`El agente Camaleón ${si_o_no(camaleonEsRojo)} es de color rojo`, camaleonEsRojo);
  res.write(`El agente Camaleón ${si_o_no(camaleonRespondeQOnda)} responde 'q onda?' si le piden repetir 'q onda?'`, camaleonRespondeQOnda);

  // Completar
  //los agentes especiales pueden hacer lo mismo que los otros agentes
  res.write("\n|| Los agentes especiales pueden espiar como el resto de los agentes ||\n");
  control = new Agencia(function() { }, "idC", "nC");
  p.espiar(control);
  let p_conoce_idC = "idC" in p;
  let p_conoce_nC = "nC" in p;
  res.write("El espía de Control" + si_o_no(p_conoce_idC) + "sabe responder idC", !p_conoce_idC);
  res.write("El espía de Control" + si_o_no(p_conoce_nC) + "sabe responder nC", p_conoce_nC);
  p.dejarDeEspiar();
  p_conoce_nC = "nC" in p;
  res.write("El espía de Control" + si_o_no(p_conoce_idC) + "sabe responder idC", !p_conoce_idC);
  res.write("El espía de Control" + si_o_no(p_conoce_nC) + "sabe responder nC", !p_conoce_nC);
}
