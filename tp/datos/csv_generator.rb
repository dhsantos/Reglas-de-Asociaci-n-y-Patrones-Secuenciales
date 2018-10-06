require 'csv'
require 'i18n'
I18n.config.available_locales = :en

# Creo el archivo que va a guardar el csv de salida
csv_out = File.new("out.csv", "w")

VOTACIONES_DIR = "votaciones/"
d = Dir.new(VOTACIONES_DIR)

# Keys de las votaciones (CSV creado por el gobierno)
keys = ["APELLIDO", "NOMBRE", "BLOQUE", "PROVINCIA", "VOTO"]

# Keys del csv que queremos generar
csv_keys = ["Coalicion Civica", "Concertacion FORJA", "Cordoba Federal", "Cordoba Trabajo y Produccion", "Cultura Educacion y Trabajo", "Elijo Catamarca", "Evolucion Radical", "Federal Unidos por una Nueva Argentina", "Frente Civico por Santiago", "Frente de la Concordia Misionero", "Frente para la Victoria - PJ", "Fte. Civico y Social de Catamarca", "Fte. de Izquierda y de los Trabajadores", "Justicialista", "Justicialista por Tucuman", "Libres del Sur", "Movimiento Popular Neuquino", "Nuevo Espacio Santafesino", "Partido Bloquista de San Juan", "Partido Intransigente de Mendoza", "Partido por la Justicia Social", "Partido Socialista", "Peronismo para la Victoria", "Primero Argentina", "PRO", "PTS - Frente de Izquierda", "Salta Somos Todos", "Somos Mendoza", "Somos San Juan", "Todos Juntos por San Juan", "Trabajo y Dignidad", "Unidad Justicialista", "Union Civica Radical", "Buenos Aires", "C.A.B.A.", "Catamarca", "Chaco", "Chubut", "Cordoba", "Corrientes", "Entre Rios", "Formosa", "Jujuy", "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquen", "Rio Negro", "Salta", "San Juan", "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero", "Tierra del Fuego", "Tucuman"]

correccionesDePartido = {"Frente Progresista Civico y Social" => "Partido Socialista", "Protectora" => "Partido Intransigente de Mendoza"}

# Creo un struct con positivo, negativo, ausente y abstencion para guardar los votos
Votos = Struct.new(:positivo, :negativo, :ausente, :abstencion)

# Por cada archivo e hago:
d.entries.select do |e| 
    # Si es un .csv
    if (/^.+\.csv$/.match(e)) then
        puts "Procesando el archivo #{e}"
        
        # Remuevo la extension
        nombreDeArcivo = File.basename(e,File.extname(e))

        # Creo el diccionario que va a guardar los votos de los bloques y provincias
        hashDeVotos = Hash.new

        # Creo los contadores que van a decir si la ley fue aprobada o rechazada
        leyAprobadaORechazada = 0

        # Obtengo las filas como un array de diccionarios
        rows = CSV.parse(File.read(VOTACIONES_DIR + e), :headers => false, :quote_char => "|").map { |a| Hash[ keys.zip(a) ] }
        
        # Remuevo la primer fila (tiene los headers del CSV)
        rows.shift

        # Por cada fila row hago:
        rows.each do |row|
            # Remuevo tildes y comillas
            partido = I18n.transliterate(row["BLOQUE"].gsub('"', ''))
            provincia = I18n.transliterate(row["PROVINCIA"].gsub('"', ''))
            voto = I18n.transliterate(row["VOTO"].gsub('"', ''))

            # Veo si el partido cambio de nombre y lo junto con el otro en caso que sea asi
            if correccionesDePartido.has_key?(partido) then
                partido = correccionesDePartido[partido]
            end

            # Si un partido o provincia no esta en las keys del csv a armar tengo que frenar todo
            if !csv_keys.include?(partido) then
                raise Exception.new("El partido #{partido} no esta en las keys del csv")
            end
            if !csv_keys.include?(provincia) then
                raise Exception.new("La provincia #{provincia} no esta en las keys del csv")
            end

            # Si el hash de votos no tiene al bloque o provincia, le creo un struct de votos        
            if !hashDeVotos.has_key?(partido) then
                hashDeVotos[partido] = Votos.new(0,0,0,0)
            end
            if !hashDeVotos.has_key?(provincia) then
                hashDeVotos[provincia] = Votos.new(0,0,0,0)
            end

            # Guardo los votos
            votosDePartido = hashDeVotos[partido]
            votosDeProvincia = hashDeVotos[provincia]

            # Leo el voto
            case voto
            when "AFIRMATIVO"
                votosDePartido.positivo += 1
                votosDeProvincia.positivo += 1
                leyAprobadaORechazada += 1
            when "NEGATIVO"
                votosDePartido.negativo += 1
                votosDeProvincia.negativo += 1
                leyAprobadaORechazada -= 1
            when "AUSENTE"
                votosDePartido.ausente += 1
                votosDeProvincia.ausente += 1
            when "ABSTENCION"
                votosDePartido.abstencion += 1
                votosDeProvincia.abstencion += 1
            when "PRESIDENTE"
                # El presidente no vota, no hago nada
            else
                raise Exception.new("No puedo leer el voto #{voto}")
            end

            # Guardo el voto
            hashDeVotos[partido] = votosDePartido
            hashDeVotos[provincia] = votosDeProvincia
        end

        csv_out.write("#{nombreDeArcivo},")
        
        if leyAprobadaORechazada >= 0 then
            csv_out.write("LEY_APROBADA,")
        else
            csv_out.write("LEY_RECHAZADA,")
        end


        csv_keys.each do |key|
            # Voy a imprimir en el csv como nombreDeBloqueOProvincia[resultadoDeSuVotacion]
            comoImprimoEnCsv = ""

            voto = hashDeVotos[key]
            if voto == nil then
                raise Exception.new("El/la bloque/provincia #{key} no fue cargada en el hash de votos")
            end

            max = [voto.positivo, voto.negativo, voto.ausente, voto.abstencion].max
            cantMax = 0
            if voto.positivo == max then
                cantMax += 1
                comoImprimoEnCsv = "POSITIVO"
            end
            if voto.negativo == max then
                cantMax +=1
                comoImprimoEnCsv = "NEGATIVO"
            end
            if voto.ausente == max then
                cantMax += 1
                comoImprimoEnCsv = "AUSENTE"
            end
            if voto.abstencion == max then
                cantMax += 1
                comoImprimoEnCsv = "ABSTENCION"
            end

            if cantMax > 1 then
                comoImprimoEnCsv = "EMPATE"
            end

            csv_out.write("#{key}[#{comoImprimoEnCsv}],")

        end

        csv_out.write("\n")
    # Termino de procesar archivo    
    end
end

csv_out.close