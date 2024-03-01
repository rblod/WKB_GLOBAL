#!/bin/bash

# Demande de confirmation
read -p "Do you want to suppress files wkb.output_p* bathy_p* namelist_p*? (o/n) " response

response=${response:-o}


# Vérification de la réponse de l'utilisateur
if [[ "$response" =~ ^[oO]$ ]]; then
    # Suppression des fichiers
    rm -f wkb.output_p* bathy_p* namelist_p*
else
    echo "Suppression cancelled."
fi
