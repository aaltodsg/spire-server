# -*- coding: utf-8 -*-
# Set the base IRI:
BASE <http://instans.cs.hut.fi/spire_repository/>

PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>
PREFIX park: <http://spitfire-project.eu/cc/parking>
PREFIX geo:  <http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX spire_data: <http://instans.cs.hut.fi/spire_data#>
PREFIX spire:<http://instans.cs.hut.fi/spire_ontology#>
PREFIX vso:  <http://www.heppnetz.de/ontologies/vso/ns#>
PREFIX tl: <http://purl.org/NET/c4dm/timeline.owl#>

INSERT DATA {

# Static global data
GRAPH <http://instans.cs.hut.fi/spire_repository/static_global> {

spire_data:parameters a spire:parameterSet ;
    spire:outstandingPushMessageLimit 3 ;
    spire:clientTimeoutAfterRequestLimit 5 . # hours

} # end of static_global

# Static user data
GRAPH <http://instans.cs.hut.fi/spire_repository/static_user> {

spire_data:testuser_gmail_com a spire:user ;
       foaf:mbox <mailto:testuser@gmail.com> ;
       spire:hasParkingPermit spire_data:AaltoUniversityEmployee .

} # end of static_user

# Static parking area data
GRAPH <http://instans.cs.hut.fi/spire_repository/static_area> {

# Otaniemi parking areas

spire_data:lat60_1882361367_long24_8100336161 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Vaisalantie, On-street parking"@eng ;
    rdfs:comment "Maximum parking time 24h"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1882361367 ; geo:long 24.8100336161 ; ] ;
    park:size 26 ;
    spire:maxParkingTime 24 ; # hours, always valid
    spire:parkingAreaRadius 79.1174 . # meters

spire_data:lat60_1874265596_long24_834031303 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Otakaari, On-street parking; 6"@eng ;
    rdfs:comment "Maximum parking time 6h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1874265596 ; geo:long 24.834031303 ; ] ;
    park:size 20 ;
    spire:maxParkingTime 6 ; # hours, always valid
    spire:parkingAreaRadius 33.6423 . # meters

spire_data:lat60_1868650176_long24_8332985742 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Otakaari, On-street parking; 5"@eng ;
    rdfs:comment "Maximum parking time 6h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1868650176 ; geo:long 24.8332985742 ; ] ;
    park:size 22 ;
    spire:maxParkingTime 6 ; # hours, always valid
    spire:parkingAreaRadius 35.817 . # meters

spire_data:lat60_1863137461_long24_8326576342 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Otakaari, On-street parking; 4"@eng ;
    rdfs:comment "Maximum parking time 6h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1863137461 ; geo:long 24.8326576342 ; ] ;
    park:size 24 ;
    spire:maxParkingTime 6 ; # hours, always valid
    spire:parkingAreaRadius 37.8081 . # meters

spire_data:lat60_1857416495_long24_8316457109 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Otakaari, On-street parking; 3"@eng ;
    rdfs:comment "Only on the western side of the road; Maximum parking time 6h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1857416495 ; geo:long 24.8316457109 ; ] ;
    park:size 16 ;
    spire:maxParkingTime 6 ; # hours, always valid
    spire:parkingAreaRadius 47.7756 . # meters

spire_data:lat60_1851578216_long24_8310176043 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Otakaari, On-street parking; 2"@eng ;
    rdfs:comment "Maximum parking time 6h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1851578216 ; geo:long 24.8310176043 ; ] ;
    park:size 17 ;
    spire:maxParkingTime 6 ; # hours, always valid
    spire:parkingAreaRadius 28.0043 . # meters

spire_data:lat60_184637067_long24_8309992448 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Otakaari, On-street parking; 1"@eng ;
    rdfs:comment "Only on the eastern side of the road; Maximum parking time 6h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.184637067 ; geo:long 24.8309992448 ; ] ;
    park:size 10 ;
    spire:maxParkingTime 6 ; # hours, always valid
    spire:parkingAreaRadius 31.173 . # meters

spire_data:lat60_1831901737_long24_8304683063 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Protoshop, Customer parking"@eng ;
    rdfs:comment "Only for the customers of Protoshop"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1831901737 ; geo:long 24.8304683063 ; ] ;
    park:size 5 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 8.48223 . # meters

spire_data:lat60_1823358382_long24_8298900698 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Metallimiehenkuja, On-street parking; 3"@eng ;
    rdfs:comment "Only on the western side of the road; Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1823358382 ; geo:long 24.8298900698 ; ] ;
    park:size 2 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 5.14629 . # meters

spire_data:lat60_1825283689_long24_8295290362 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Metallimiehenkuja, On-street parking; 2"@eng ;
    rdfs:comment "Only on the western side of the road; Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1825283689 ; geo:long 24.8295290362 ; ] ;
    park:size 3 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 11.4582 . # meters

spire_data:lat60_1827715454_long24_8290533105 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Metallimiehenkuja, On-street parking; 1"@eng ;
    rdfs:comment "Only on the western side of the road; Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1827715454 ; geo:long 24.8290533105 ; ] ;
    park:size 4 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 16.0723 . # meters

spire_data:lat60_181569381_long24_8259405129 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Puu 1 Building, Personnel parking; 1"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.181569381 ; geo:long 24.8259405129 ; ] ;
    park:size 40 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 25.2535 . # meters

spire_data:lat60_1815182696_long24_8262147951 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Puu 1 Building, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1815182696 ; geo:long 24.8262147951 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 6.00935 . # meters

spire_data:lat60_1784588615_long24_8289571891 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Miestentie 3 Building, Guest parking"@eng ;
    rdfs:comment "Only for the visitors and personnel of the building"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1784588615 ; geo:long 24.8289571891 ; ] ;
    park:size 78 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 59.3237 . # meters

spire_data:lat60_1782408331_long24_8288129486 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Miestentie 3 Building, Disabled parking; 1"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1782408331 ; geo:long 24.8288129486 ; ] ;
    park:size 2 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 9.19627 . # meters

spire_data:lat60_1785314846_long24_828639826 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Miestentie 3 Building, Disabled parking; 2"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1785314846 ; geo:long 24.828639826 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 7.1016 . # meters

spire_data:lat60_1812150781_long24_8308938313 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University,  Aalto Art House, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1812150781 ; geo:long 24.8308938313 ; ] ;
    park:size 13 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 23.1416 . # meters

spire_data:lat60_1817710388_long24_832715114 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Lämpömiehenkuja 2, Personnel parking"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1817710388 ; geo:long 24.832715114 ; ] ;
    park:size 26 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 35.629 . # meters

spire_data:lat60_1819393332_long24_8324969885 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Lämpömiehenkuja 2, Disabled parking; 2"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1819393332 ; geo:long 24.8324969885 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 5.12776 . # meters

spire_data:lat60_1907398615_long24_8318610522 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Finnish Student Health Service, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1907398615 ; geo:long 24.8318610522 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 7.44474 . # meters

spire_data:lat60_1906238002_long24_8317612286 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Finnish Student Health Service, Guest parking"@eng ;
    rdfs:comment "Only for the customers of FSHS"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1906238002 ; geo:long 24.8317612286 ; ] ;
    park:size 8 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 12.1668 . # meters

spire_data:lat60_1855066658_long24_8370611642 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Otaranta, Guest parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1855066658 ; geo:long 24.8370611642 ; ] ;
    park:size 132 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 52.8111 . # meters

spire_data:lat60_1794178852_long24_8296240853 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Geological Survey of Finland, Guest parking; 2"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1794178852 ; geo:long 24.8296240853 ; ] ;
    park:size 16 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 16.0342 . # meters

spire_data:lat60_1799054997_long24_83248712 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Startup Sauna, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1799054997 ; geo:long 24.83248712 ; ] ;
    park:size 17 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 24.1186 . # meters

spire_data:lat60_1802773692_long24_8321788369 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Startup Sauna, Personnel parking; 3"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1802773692 ; geo:long 24.8321788369 ; ] ;
    park:size 7 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 12.8014 . # meters

spire_data:lat60_1808132705_long24_832072669 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Startup Sauna, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1808132705 ; geo:long 24.832072669 ; ] ;
    park:size 16 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 46.4305 . # meters

spire_data:lat60_1807186477_long24_8318335422 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Startup Sauna, Personnel parking; 1"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1807186477 ; geo:long 24.8318335422 ; ] ;
    park:size 7 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 15.3455 . # meters

spire_data:lat60_1807220614_long24_8327088497 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Startup Sauna, Personnel parking; 2"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1807220614 ; geo:long 24.8327088497 ; ] ;
    park:size 7 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 15.5224 . # meters

spire_data:lat60_1804542974_long24_8289492864 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Geological Survey of Finland, Guest parking; 1"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1804542974 ; geo:long 24.8289492864 ; ] ;
    park:size 19 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 15.5075 . # meters

spire_data:lat60_1807909722_long24_8261354129 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Puu 1 Building, Guest parking"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1807909722 ; geo:long 24.8261354129 ; ] ;
    park:size 8 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 13.1722 . # meters

spire_data:lat60_180897279_long24_8250737728 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Puu 2 Building, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.180897279 ; geo:long 24.8250737728 ; ] ;
    park:size 7 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 10.7075 . # meters

spire_data:lat60_1807589936_long24_8251589479 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Puu 2 Building, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1807589936 ; geo:long 24.8251589479 ; ] ;
    park:size 1 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 4.87403 . # meters

spire_data:lat60_1808015407_long24_8251258638 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Puu 2 Building, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1808015407 ; geo:long 24.8251258638 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 4.02803 . # meters

spire_data:lat60_1811216439_long24_8279438776 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Kivimiehentie"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1811216439 ; geo:long 24.8279438776 ; ] ;
    park:size 10 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 32.0592 . # meters

spire_data:lat60_1813025999_long24_8320022561 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Design Factory, Guest parking"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1813025999 ; geo:long 24.8320022561 ; ] ;
    park:size 15 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 21.991 . # meters

spire_data:lat60_1821670181_long24_8320335839 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Lämpömiehenkuja 2, Disabled parking; 1"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1821670181 ; geo:long 24.8320335839 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 5.79739 . # meters

spire_data:lat60_1825194005_long24_8320026265 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Lämpömiehenkuja 2, Guest parking"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1825194005 ; geo:long 24.8320026265 ; ] ;
    park:size 22 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 21.6168 . # meters

spire_data:lat60_1822323735_long24_8304497497 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Metallimiehenkuja 10, Personnel parking"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1822323735 ; geo:long 24.8304497497 ; ] ;
    park:size 5 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 9.9486 . # meters

spire_data:lat60_1823023902_long24_8302365059 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Metallimiehenkuja 10, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1823023902 ; geo:long 24.8302365059 ; ] ;
    park:size 2 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 6.52287 . # meters

spire_data:lat60_1818043278_long24_8299729541 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Aalto Art House, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1818043278 ; geo:long 24.8299729541 ; ] ;
    park:size 25 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 31.4357 . # meters

spire_data:lat60_1817480168_long24_8288372877 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Aalto Art House, Guest parking; 3"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1817480168 ; geo:long 24.8288372877 ; ] ;
    park:size 25 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 35.8458 . # meters

spire_data:lat60_181160455_long24_825968541 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Puu 1 Building, Personnel parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.181160455 ; geo:long 24.825968541 ; ] ;
    park:size 6 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 17.0535 . # meters

spire_data:lat60_1888464317_long24_8092662628 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Falcon Gentti, Guest parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1888464317 ; geo:long 24.8092662628 ; ] ;
    park:size 41 ;
    spire:maxParkingTime 2 ; # hours, always valid
    spire:parkingAreaRadius 27.8854 . # meters

spire_data:lat60_1888460427_long24_8088407915 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Falcon Gentti, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1888460427 ; geo:long 24.8088407915 ; ] ;
    park:size 4 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 15.5397 . # meters

spire_data:lat60_1853842434_long24_8128955977 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Technopolis, Innopoli, Disabled parking; 1"@eng ;
    rdfs:comment "Boom gates, exit tickets from the reception"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1853842434 ; geo:long 24.8128955977 ; ] ;
    park:size 2 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 6.39018 . # meters

spire_data:lat60_1848323045_long24_8136910734 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Technopolis, Innopoli, Disabled parking; 2"@eng ;
    rdfs:comment "Boom gates, exit tickets from the reception"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1848323045 ; geo:long 24.8136910734 ; ] ;
    park:size 2 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 7.29229 . # meters

spire_data:lat60_1846445576_long24_8142595204 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Technopolis, Innopoli, Disabled parking; 3"@eng ;
    rdfs:comment "Boom gates, exit tickets from the reception"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1846445576 ; geo:long 24.8142595204 ; ] ;
    park:size 2 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 7.4592 . # meters

spire_data:lat60_1850241144_long24_8136363557 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Technopolis, Innopoli, Guest parking"@eng ;
    rdfs:comment "Boom gates, exit tickets from the reception"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1850241144 ; geo:long 24.8136363557 ; ] ;
    park:size 135 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 67.731 . # meters

spire_data:lat60_1825967264_long24_8255220074 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Material Science Building, Personnel parking;1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1825967264 ; geo:long 24.8255220074 ; ] ;
    park:size 23 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 29.4644 . # meters

spire_data:lat60_1826746475_long24_8259370591 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Material Science Building, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1826746475 ; geo:long 24.8259370591 ; ] ;
    park:size 10 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 19.095 . # meters

spire_data:lat60_1828105472_long24_825950875 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Material Science Building, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1828105472 ; geo:long 24.825950875 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 5.38961 . # meters

spire_data:lat60_1829415398_long24_8237857259 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Chemical Technology, Personnel parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1829415398 ; geo:long 24.8237857259 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 5.58491 . # meters

spire_data:lat60_1829472705_long24_82352851 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Chemical Technology, Personnel parking; 3"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1829472705 ; geo:long 24.82352851 ; ] ;
    park:size 3 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 19.4969 . # meters

spire_data:lat60_1819352625_long24_8234131376 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Kemistintie, Guest parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1819352625 ; geo:long 24.8234131376 ; ] ;
    park:size 21 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 30.146 . # meters

spire_data:lat60_1821308731_long24_8247333094 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Material Science Building, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1821308731 ; geo:long 24.8247333094 ; ] ;
    park:size 11 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 19.4353 . # meters

spire_data:lat60_1826727247_long24_8250698017 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Material Science Building, Personnel parking;2"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1826727247 ; geo:long 24.8250698017 ; ] ;
    park:size 5 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 16.4328 . # meters

spire_data:lat60_1824724032_long24_8243135904 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Material Science Building, Personnel parking;3"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1824724032 ; geo:long 24.8243135904 ; ] ;
    park:size 15 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 33.5835 . # meters

spire_data:lat60_1821969312_long24_8241504834 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Material Science Building, Personnel parking;4"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1821969312 ; geo:long 24.8241504834 ; ] ;
    park:size 17 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 33.056 . # meters

spire_data:lat60_1826318119_long24_8239480077 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Chemical Technology, Personnel parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1826318119 ; geo:long 24.8239480077 ; ] ;
    park:size 8 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 18.7814 . # meters

spire_data:lat60_1832728061_long24_8242971043 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Chemical Technology, Personnel parking; 4"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1832728061 ; geo:long 24.8242971043 ; ] ;
    park:size 4 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 27.7581 . # meters

spire_data:lat60_1835015572_long24_8250503856 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Chemical Technology, Personnel parking; 5"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1835015572 ; geo:long 24.8250503856 ; ] ;
    park:size 3 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 24.6529 . # meters

spire_data:lat60_1837266332_long24_825812963 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Chemical Technology, Personnel parking; 6"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1837266332 ; geo:long 24.825812963 ; ] ;
    park:size 3 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 17.4291 . # meters

spire_data:lat60_1850264914_long24_8156205785 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Micronova"@eng ;
    rdfs:comment "Only for the visitors and personnel of Micronova"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1850264914 ; geo:long 24.8156205785 ; ] ;
    park:size 150 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 64.4669 . # meters

spire_data:lat60_1889698807_long24_8273672664 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Maarintalo, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1889698807 ; geo:long 24.8273672664 ; ] ;
    park:size 45 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 24.8274 . # meters

spire_data:lat60_189064805_long24_8270917063 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Maarintalo, Guest parking; 2"@eng ;
    rdfs:comment "Only for the customers of Terveystalo; Maximum parking time 4h on weekdays; Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.189064805 ; geo:long 24.8270917063 ; ] ;
    park:size 13 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 21.927 . # meters

spire_data:lat60_1870875436_long24_8256032584 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Puumiehenkuja, Personnel parking"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1870875436 ; geo:long 24.8256032584 ; ] ;
    park:size 68 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 53.7852 . # meters

# Inside the loop- and camera-monitored area below

spire_data:lat60_1875848336_long24_8145678732 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, OIH, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1875848336 ; geo:long 24.8145678732 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 7.98094 . # meters

# Replicating the OIH parking lot to provide both loop-monitored and camera-monitored figures for comparison.
# Not exactly in the center of the parking area to deviate from the camera-monitored symbol.

spire_data:lat60_187554_long24_815868 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, OIH, Loop"@eng ;
    rdfs:comment "Loop-monitored area. Also camera monitoring for the same area is available."@eng ;
    spire:parkingAreaLocation [ geo:lat 60.187554 ; geo:long 24.815868 ; ] ;
    park:size 83 ;
    spire:sensor "Loop-P2" ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 72.8694 . # meters

spire_data:lat60_1875788859_long24_8154725888 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, OIH, Camera"@eng ;
    rdfs:comment "Camera-monitored area. Also loop monitoring for the same area is available."@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1875788859 ; geo:long 24.8154725888 ; ] ;
    park:size 83 ;
    spire:sensor "Camera1-OIH" ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 72.8694 . # meters

# Co-located with the loop-monitored area below

spire_data:lat60_1862596477_long24_8206116057 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Maritime Technology, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1862596477 ; geo:long 24.8206116057 ; ] ;
    park:size 3 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 8.35776 . # meters

# Loop-monitored area

spire_data:lat60_1862504373_long24_8204088678 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Maritime Technology, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays. Automatic loop monitoring."@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1862504373 ; geo:long 24.8204088678 ; ] ;
    park:size 97 ;
    spire:sensor "Loop-P1" ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 79.4847 . # meters

spire_data:lat60_1898511762_long24_8318696561 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 5, Personnel parking; 3"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1898511762 ; geo:long 24.8318696561 ; ] ;
    park:size 53 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 43.1929 . # meters

spire_data:lat60_1896056512_long24_830566573 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 5, Personnel parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1896056512 ; geo:long 24.830566573 ; ] ;
    park:size 25 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 19.5262 . # meters

spire_data:lat60_1895668855_long24_830482459 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 5, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1895668855 ; geo:long 24.830482459 ; ] ;
    park:size 20 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 25.7426 . # meters

spire_data:lat60_1893451616_long24_8305761334 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 5, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1893451616 ; geo:long 24.8305761334 ; ] ;
    park:size 17 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 31.5432 . # meters

spire_data:lat60_1894651851_long24_8312361363 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 5, Personnel parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1894651851 ; geo:long 24.8312361363 ; ] ;
    park:size 7 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 9.1036 . # meters

spire_data:lat60_1894650179_long24_8310248369 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 5, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1894650179 ; geo:long 24.8310248369 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 4.06412 . # meters

spire_data:lat60_1891360162_long24_8329677357 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Electrical Eng., Personnel parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1891360162 ; geo:long 24.8329677357 ; ] ;
    park:size 48 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 72.265 . # meters

spire_data:lat60_189210217_long24_833766336 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Electrical Eng., Guest parking"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.189210217 ; geo:long 24.833766336 ; ] ;
    park:size 8 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 19.3484 . # meters

spire_data:lat60_1892121486_long24_8335677934 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Electrical Eng., Personnel parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1892121486 ; geo:long 24.8335677934 ; ] ;
    park:size 12 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 19.2978 . # meters

spire_data:lat60_188895574_long24_8264433896 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Maarintalo, Guest parking; 3"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.188895574 ; geo:long 24.8264433896 ; ] ;
    park:size 2 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 7.0057 . # meters

spire_data:lat60_1883170521_long24_8282932695 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Sähkömiehentie 2, Guest parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1883170521 ; geo:long 24.8282932695 ; ] ;
    park:size 13 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 24.7226 . # meters

spire_data:lat60_1890447421_long24_8288303274 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Sähkömiehenpolku, Guest parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1890447421 ; geo:long 24.8288303274 ; ] ;
    park:size 74 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 44.9417 . # meters

spire_data:lat60_1877782281_long24_8305297516 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 3, Personnel parking; 3"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1877782281 ; geo:long 24.8305297516 ; ] ;
    park:size 5 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 14.3027 . # meters

spire_data:lat60_1871493334_long24_8320243006 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Rakentajanaukio 4, Personnel parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1871493334 ; geo:long 24.8320243006 ; ] ;
    park:size 14 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 16.7772 . # meters

spire_data:lat60_1875022586_long24_8318756339 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Rakentajanaukio 4, Personnel parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1875022586 ; geo:long 24.8318756339 ; ] ;
    park:size 3 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 9.67058 . # meters

spire_data:lat60_188135379_long24_8308702289 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 3, Personnel parking; 2"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.188135379 ; geo:long 24.8308702289 ; ] ;
    park:size 3 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 24.2056 . # meters

spire_data:lat60_1880173349_long24_831120099 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 3, Personnel parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1880173349 ; geo:long 24.831120099 ; ] ;
    park:size 32 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 37.6979 . # meters

spire_data:lat60_1883837733_long24_8297496845 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 3, Guest parking"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1883837733 ; geo:long 24.8297496845 ; ] ;
    park:size 3 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 6.42498 . # meters

spire_data:lat60_1874847113_long24_8295342397 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Rakentajanaukio, Personnel parking"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1874847113 ; geo:long 24.8295342397 ; ] ;
    park:size 77 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 69.2268 . # meters

spire_data:lat60_1867408024_long24_8302690464 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Rakentajanaukio, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1867408024 ; geo:long 24.8302690464 ; ] ;
    park:size 3 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 23.6988 . # meters

spire_data:lat60_1869300964_long24_830282383 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Rakentajanaukio, Guest parking; 3"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1869300964 ; geo:long 24.830282383 ; ] ;
    park:size 24 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 43.3327 . # meters

spire_data:lat60_1877292433_long24_8291031221 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Rakentajanaukio, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1877292433 ; geo:long 24.8291031221 ; ] ;
    park:size 14 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 24.1849 . # meters

spire_data:lat60_1843420193_long24_8340544892 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Otahalli, Guest parking"@eng ;
    rdfs:comment "Maximum parking time 24h"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1843420193 ; geo:long 24.8340544892 ; ] ;
    park:size 80 ;
    spire:maxParkingTime 24 ; # hours, always valid
    spire:parkingAreaRadius 46.0483 . # meters

spire_data:lat60_1855960723_long24_8326857876 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Dipoli, Guest parking; 2"@eng ;
    rdfs:comment "Only for the visitors of Dipoli; Maximum parking time 24h"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1855960723 ; geo:long 24.8326857876 ; ] ;
    park:size 21 ;
    spire:maxParkingTime 24 ; # hours, always valid
    spire:parkingAreaRadius 38.3287 . # meters

spire_data:lat60_1843400734_long24_8319741415 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Dipoli, Guest parking; 1"@eng ;
    rdfs:comment "Only for the visitors of Dipoli; Maximum parking time 24h"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1843400734 ; geo:long 24.8319741415 ; ] ;
    park:size 95 ;
    spire:maxParkingTime 24 ; # hours, always valid
    spire:parkingAreaRadius 37.9357 . # meters

spire_data:lat60_1844618255_long24_8320764496 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Dipoli, Disabled parking"@eng ;
    rdfs:comment "Only for the visitors of Dipoli; Maximum parking time 24h"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1844618255 ; geo:long 24.8320764496 ; ] ;
    park:size 2 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 24 ; # hours, always valid
    spire:parkingAreaRadius 5.35205 . # meters

spire_data:lat60_1839400191_long24_8306558675 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Otaniemi Mall, Guest parking"@eng ;
    rdfs:comment "Maximum parking time 60 min on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1839400191 ; geo:long 24.8306558675 ; ] ;
    park:size 38 ;
    spire:maxParkingTime 1 ; # hours, always valid
    spire:parkingAreaRadius 54.2461 . # meters

spire_data:lat60_1841907535_long24_8290023809 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otaniemi Campus Library, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1841907535 ; geo:long 24.8290023809 ; ] ;
    park:size 11 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 20.3603 . # meters

spire_data:lat60_1842091688_long24_8292364544 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otaniemi Campus Library, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1842091688 ; geo:long 24.8292364544 ; ] ;
    park:size 10 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 14.514 . # meters

spire_data:lat60_184357227_long24_8289608991 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otaniemi Campus Library, Personnel parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.184357227 ; geo:long 24.8289608991 ; ] ;
    park:size 5 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 7.89902 . # meters

spire_data:lat60_1844106138_long24_8288585645 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otaniemi Library, Disabled parking; 2"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1844106138 ; geo:long 24.8288585645 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 4.84058 . # meters

spire_data:lat60_1845310656_long24_8281120111 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otaniemi Library, Disabled parking; 1"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1845310656 ; geo:long 24.8281120111 ; ] ;
    park:size 2 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 9.4115 . # meters

spire_data:lat60_1860138148_long24_8265549758 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 1, Personnel parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1860138148 ; geo:long 24.8265549758 ; ] ;
    park:size 12 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 29.6 . # meters

spire_data:lat60_1855271575_long24_8265658168 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 1, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1855271575 ; geo:long 24.8265658168 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 4.4609 . # meters

spire_data:lat60_185436339_long24_8265031246 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Otakaari 1, Guest parking"@eng ;
    rdfs:comment "Maximum parking time 30 min on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.185436339 ; geo:long 24.8265031246 ; ] ;
    park:size 5 ;
    spire:maxParkingTime 0.5 ; # hours, always valid
    spire:parkingAreaRadius 9.88767 . # meters

spire_data:lat60_1875688589_long24_8227921504 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Konemiehentie 4, Personnel parking"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1875688589 ; geo:long 24.8227921504 ; ] ;
    park:size 4 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 10.4548 . # meters

spire_data:lat60_1881059586_long24_8241231934 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Mechanical Eng. 3, Personnel parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays; Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1881059586 ; geo:long 24.8241231934 ; ] ;
    park:size 6 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 37.4652 . # meters

spire_data:lat60_1895146538_long24_8254487167 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Maarintalo, Personnel parking"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1895146538 ; geo:long 24.8254487167 ; ] ;
    park:size 8 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 15.3551 . # meters

spire_data:lat60_1891356101_long24_8248084215 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Mechanical Eng. 4, Personnel parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1891356101 ; geo:long 24.8248084215 ; ] ;
    park:size 24 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 25.0662 . # meters

spire_data:lat60_1886964159_long24_8242608637 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Mechanical Eng. 3, Personnel parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1886964159 ; geo:long 24.8242608637 ; ] ;
    park:size 13 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 24.5705 . # meters

spire_data:lat60_1885848366_long24_8254094007 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Mechanical Eng. 4, Personnel parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1885848366 ; geo:long 24.8254094007 ; ] ;
    park:size 6 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 11.115 . # meters

spire_data:lat60_1880131108_long24_824961473 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Mechanical Eng. 2, Personnel parking"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays; Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1880131108 ; geo:long 24.824961473 ; ] ;
    park:size 49 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 47.6729 . # meters

spire_data:lat60_1875821744_long24_8240087234 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Saha, Guest parking"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1875821744 ; geo:long 24.8240087234 ; ] ;
    park:size 10 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 11.554 . # meters

spire_data:lat60_1876607681_long24_8237339249 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Saha, Personnel parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1876607681 ; geo:long 24.8237339249 ; ] ;
    park:size 4 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 7.40987 . # meters

spire_data:lat60_1870944235_long24_8253237475 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Nanotalo, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1870944235 ; geo:long 24.8253237475 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 4.61694 . # meters

spire_data:lat60_1872646987_long24_8238923495 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Nanotalo, Personnel parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1872646987 ; geo:long 24.8238923495 ; ] ;
    park:size 16 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 20.233 . # meters

spire_data:lat60_1870043676_long24_8239490819 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Nanotalo, Personnel parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1870043676 ; geo:long 24.8239490819 ; ] ;
    park:size 6 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 13.5079 . # meters

spire_data:lat60_1859041403_long24_8190233164 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Maritime Technology, Personnel parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1859041403 ; geo:long 24.8190233164 ; ] ;
    park:size 34 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 45.5576 . # meters

spire_data:lat60_1858915716_long24_8183563399 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Maritime Technology, Personnel parking; 2"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1858915716 ; geo:long 24.8183563399 ; ] ;
    park:size 3 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 8.01068 . # meters

spire_data:lat60_1859027419_long24_8198728254 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Maritime Technology, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1859027419 ; geo:long 24.8198728254 ; ] ;
    park:size 6 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 16.1655 . # meters

spire_data:lat60_1853945016_long24_8202589675 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Water Laboratory, Personnel parking"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1853945016 ; geo:long 24.8202589675 ; ] ;
    park:size 1 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 19.5801 . # meters

spire_data:lat60_1847403387_long24_8243506684 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Biologinkuja, Guest parking"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1847403387 ; geo:long 24.8243506684 ; ] ;
    park:size 29 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 23.5264 . # meters

spire_data:lat60_1854021795_long24_8243899872 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Korkeakouluaukio, Guest parking; 3"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1854021795 ; geo:long 24.8243899872 ; ] ;
    park:size 96 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 38.0388 . # meters

spire_data:lat60_1859431732_long24_8232289899 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Korkeakouluaukio, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1859431732 ; geo:long 24.8232289899 ; ] ;
    park:size 94 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 37.102 . # meters

spire_data:lat60_1857361212_long24_8233460148 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Korkeakouluaukio, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1857361212 ; geo:long 24.8233460148 ; ] ;
    park:size 13 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 17.0516 . # meters

spire_data:lat60_1865757902_long24_8236119821 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Ainon aukio, Guest parking; 1"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1865757902 ; geo:long 24.8236119821 ; ] ;
    park:size 126 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 59.325 . # meters

spire_data:lat60_1864564926_long24_8232733194 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Ainon aukio, Guest parking; 2"@eng ;
    rdfs:comment "Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1864564926 ; geo:long 24.8232733194 ; ] ;
    park:size 32 ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 38.8478 . # meters

spire_data:lat60_1862288457_long24_8241105447 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, Ainon aukio, Personnel parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1862288457 ; geo:long 24.8241105447 ; ] ;
    park:size 10 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 14.7762 . # meters

spire_data:lat60_1864745038_long24_8212250245 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Espoo, Otaniementie, Public parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1864745038 ; geo:long 24.8212250245 ; ] ;
    park:size 18 ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 22.9984 . # meters

# Merging Personnel and Guest parking areas to correspond to the loop-monitored area

#spire_data:lat60_1874840998_long24_8217108426 a park:ParkingArea ;
#    spire:areaType park:CarArea ;
#    park:id "Aalto University, Computer Science Building, Personnel parking"@eng ;
#    rdfs:comment "Maximum parking time 20h on weekdays; Some of the places are designated"@eng ;
#    spire:parkingAreaLocation [ geo:lat 60.1874840998 ; geo:long 24.8217108426 ; ] ;
#    park:size 44 ;
#    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
#    spire:maxParkingTime 20 ; # hours, always valid
#    spire:parkingAreaRadius 24.0486 . # meters

spire_data:lat60_1875823646_long24_8210595759 a park:ParkingArea ;
    spire:areaType park:CarArea ;
#    park:id "Aalto University, Computer Science Building, Guest parking"@eng ;
    park:id "Aalto University, Computer Science Building, Loop-monitored parking"@eng ;
    rdfs:comment "Loop-monitored occupancy jointly for guest and personnel parking. Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1875823646 ; geo:long 24.8210595759 ; ] ;
#    park:size 52 ;
    park:size 96 ;
    spire:sensor "Loop-P4" ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 24.6012 . # meters

spire_data:lat60_1871732661_long24_8203071269 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, TUAS Building, Guest parking; 1"@eng ;
    rdfs:comment "Loop-monitored parking area. Maximum parking time 4h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1871732661 ; geo:long 24.8203071269 ; ] ;
    park:size 15 ;
    spire:sensor "Loop-P5" ;
    spire:maxParkingTime 4 ; # hours, always valid
    spire:parkingAreaRadius 19.1632 . # meters

spire_data:lat60_1874804723_long24_8197506655 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, TUAS Building, Personnel parking; 4"@eng ;
    rdfs:comment "Some of the places are designated"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1874804723 ; geo:long 24.8197506655 ; ] ;
    park:size 11 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 30.1878 . # meters

spire_data:lat60_1875119956_long24_8190465648 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, TUAS Building, Personnel parking; 3"@eng ;
    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1875119956 ; geo:long 24.8190465648 ; ] ;
    park:size 6 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 9.18524 . # meters

spire_data:lat60_1871998366_long24_8172919916 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, TUAS Building, Personnel parking; 2"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1871998366 ; geo:long 24.8172919916 ; ] ;
    park:size 9 ;
    spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 13.6143 . # meters

# Temporarily merging the personnel parking with guest parking, because
# the camera sensor will cover both parking lots, i.e. infrastructure data
# will only be available for the combined area

# spire_data:lat60_1875691294_long24_8179224592 a park:ParkingArea ;
#     spire:areaType park:CarArea ;
#     park:id "Aalto University, TUAS Building, Personnel parking; 1"@eng ;
#     rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
#     spire:parkingAreaLocation [ geo:lat 60.1875691294 ; geo:long 24.8179224592 ; ] ;
#     park:size 75 ;
#     spire:requiresParkingPermit spire_data:AaltoUniversityEmployee ;
#     spire:maxParkingTime 20 ; # hours, always valid
#     spire:parkingAreaRadius 33.1589 . # meters

# Note: Disabled parking not merged; may cause bias in some cases

spire_data:lat60_187359755_long24_817731076 a park:ParkingArea ;
    spire:areaType park:CarArea ;
#    park:id "Aalto University, TUAS Building, Guest parking; 2"@eng ;
    park:id "Aalto University, TUAS Building, Camera-supervised parking"@eng ;
#    rdfs:comment "Maximum parking time 20h on weekdays"@eng ;
    rdfs:comment "Personnel and guest parking merged as one camera-supervised area."@eng ;
    spire:parkingAreaLocation [ geo:lat 60.187359755 ; geo:long 24.817731076 ; ] ;
#    park:size 15 ;
    park:size 90 ;
    spire:sensor "Camera1-TUAS" ; # The machine sensor label by which updates are issued.
    spire:image spire_data:camera1 ;
    spire:maxParkingTime 20 ; # hours, always valid
    spire:parkingAreaRadius 20.6804 . # meters

spire_data:lat60_1873134014_long24_8181859702 a park:ParkingArea ;
    spire:areaType park:CarArea ;
    park:id "Aalto University, TUAS Building, Disabled parking"@eng ;
    spire:parkingAreaLocation [ geo:lat 60.1873134014 ; geo:long 24.8181859702 ; ] ;
    park:size 3 ;
    spire:requiresParkingPermit spire_data:Handicapped ;
    spire:maxParkingTime 0 ; # hours, always valid
    spire:parkingAreaRadius 6.25856 . # meters

# Citat test areas in Ukraine

# spire_data:lat48_336440_long33_511181 a park:ParkingArea ;
#    spire:areaType park:CarArea ;
#    park:id "Athletic Field, Guest parking"@fin ;
#    park:id "Athletic Field, Guest parking"@eng ;
#    spire:parkingAreaLocation [ geo:lat 48.336440; geo:long 33.511181; ] ;
#    rdfs:comment "Address: Address,Espoo,<br>Parking time: 09:00 - 23:00<br>Contacts: 020 8402910"@eng ;
#    park:size 15 ;
#    spire:maxParkingTime 4 ; # hours, always valid ;
#    spire:parkingAreaRadius 300 ; # meters ;
#    spire:requiresParkingPermit spire_data:CitatSpecial .

#spire_data:lat48_345466_long33_509682 a park:ParkingArea ;
#    spire:areaType park:CarArea ;
#    park:id "Cinema Mir, Guest parking"@fin ;
#    park:id "Cinema Mir, Guest parking"@eng ;
#    spire:parkingAreaLocation [ geo:lat 48.345466; geo:long 33.509682; ] ;
#    park:size 20 ;
#    spire:maxParkingTime 4 ; # hours, always valid ;
#    spire:parkingAreaRadius 30 ; # meters ;
#    spire:requiresParkingPermit spire_data:CitatSpecial .

#spire_data:lat48_345115_long33_506569 a park:ParkingArea ;
#    spire:areaType park:CarArea ;
#    park:id "Park Palace, Guest parking"@fin ;
#    park:id "Park Palace, Guest parking"@eng ;
#    spire:parkingAreaLocation [ geo:lat 48.345115; geo:long 33.506569; ] ;
#    park:size 20 ;
#    spire:maxParkingTime 4 ; # hours, always valid ;
#    spire:parkingAreaRadius 150 . # meters ;

#spire_data:lat48_348115_long_33_508342 a park:ParkingArea ;
#    spire:areaType park:CarArea ;
#    park:id "Park Bank of Ukraine Zheltye Vody department, Client parking"@fin ;
#    park:id "Park Bank of Ukraine Zheltye Vody department, Client parking"@eng ;
#    spire:parkingAreaLocation [ geo:lat 48.348115; geo:long 33.508342; ] ;
#    park:size 10 ;
#    spire:maxParkingTime 4 ; # hours, always valid ;
#    spire:parkingAreaRadius 20 . # meters ;

#spire_data:lat50_001636_long36_251035 a park:ParkingArea ;
#    spire:areaType park:CarArea ;
#    park:id "National Technical University 'Kharkiv Polytechnical Institute', Rear parking"@fin ;
#    park:id "National Technical University 'Kharkiv Polytechnical Institute', Rear parking"@eng ;
#    spire:parkingAreaLocation [ geo:lat 50.001636; geo:long 36.251035; ] ;
#    rdfs:comment "Parking area behind the Computer Science building. Parking 20h with a permission sticker from the university."@fin ;
#    rdfs:comment "Parking area behind the Computer Science building. Parking 20h with a permission sticker from the university."@eng ;
#    park:size 30 ;
#    spire:maxParkingTime 8 ; # hours, always valid ;
#    spire:parkingAreaRadius 100 . # meters ;

} # end of static_area

# Static destination data - old for v1
GRAPH <http://instans.cs.hut.fi/spire_repository/static_destination> {

[] a spire:destinationInfo ;
   rdfs:label "Open Innovation House (OIH)"@eng ;
   rdfs:comment "Accommodates Nokia Research Center (NRC) & European Institute of Technology (EIT)"@eng ;
   geo:Point [ geo:lat 60.1876172 ; geo:long 24.815366 ; ] .

[] a spire:walkingInstructions ;
   rdfs:label "From CS building employee parking lot to OIH"@eng ;
   rdfs:comment "Walk back to Otaniementie. Turn right and walk past the TUAS building. The first door of the building after TUAS is the OIH main entrance."@eng ;
   park:ParkingArea spire_data:lat60_187519_long24_821232 ;
   spire:destinationPoint [ geo:lat 60.1876172 ; geo:long 24.815366 ; ] .

} # end of static_destination

# Static destination data - new for EnterLot v2
GRAPH <http://instans.cs.hut.fi/spire_repository/static_destination2> {

spire_data:lat60_187347_long24_815352 a spire:destinationInfo ;
   spire:areaType spire:destination ;
   rdfs:label "Open Innovation House (OIH)"@eng ;
   rdfs:comment "Main entrance is open from 8 am until 4 pm<br>In the lobby you can find<br>- reception desk service<br>- café, open from 8 to 16 (lunch soups and salads available from 11 am until 2 pm)<br>- cloakroom (non-guarded)<br>- negotiation rooms<br>- restrooms<br><br>You can also enter the lobby via back door from parking area."@eng ;
   geo:Point [ geo:lat 60.187347 ; geo:long 24.815352 ; ] ;
   spire:image spire_data:oih_entrance1 ;
   spire:image spire_data:oih_lobby .

spire_data:oih_entrance1 a spire:image ;
   geo:Point [ geo:lat 60.187099 ; geo:long 24.816414 ; ] ;
   spire:imageuri <https://drive.google.com/uc?id=0B3om5aa6_GwHQWVMc28yNTUxdXc> .

spire_data:oih_lobby a spire:image ;
   geo:Point [ geo:lat 60.187176 ; geo:long 24.816350 ; ] ;
   spire:imageuri <https://drive.google.com/uc?id=0B3om5aa6_GwHNWNrZnFvN3diaTQ> .

spire_data:camera1 a spire:image ;
   geo:Point [ geo:lat 60.187458 ; geo:long 24.818560 ; ] ;
   spire:imageuri <http://camera.address.com/events/picture.jpg> .

[] a spire:walkingInstructions ;
   park:ParkingArea spire_data:lat60_187519_long24_821232 ;
   spire:destination spire_data:lat60_187347_long24_815352 ;
   rdfs:label "From CS building employee parking lot to OIH"@eng ;
   rdfs:comment "Walk back to Otaniementie. Turn right and walk past the TUAS building. The first door of the building after TUAS is the OIH main entrance."@eng ;
   spire:image spire_data:route_image1 ;
   spire:image spire_data:route_image2 .

spire_data:route_image1 a spire:image ;
   geo:Point [ geo:lat 60.187458 ; geo:long 24.818560 ; ] ;
   spire:imageuri <http://camera.address.com/events/picture.jpg> .

spire_data:route_image2 a spire:image ;
   geo:Point [ geo:lat 60.187099 ; geo:long 24.816414 ; ] ;
   spire:imageuri <https://drive.google.com/uc?id=0B3om5aa6_GwHQWVMc28yNTUxdXc> .

} # end of static_destination2

} # end of INSERT DATA

