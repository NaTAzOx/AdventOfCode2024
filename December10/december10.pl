use strict;
use warnings;

my $filename = 'December10/input';

# Chargement de l'input
open(FH, '<', $filename) or die $!;
my @lines = <FH>;
close(FH);

# Supprime les sauts de ligne et convertit en tableau 2D
chomp @lines;
my @grid = map { [split //, $_] } @lines;

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};
my $total_rating = 0;

# Définition des directions (droite, bas, gauche, haut)
my @directions = ([0, 1], [1, 0], [0, -1], [-1, 0]);

# Vérifie si une position est dans les limites de la grille
sub in_bounds {
    my ($x, $y) = @_;
    return $x >= 0 && $x < $cols && $y >= 0 && $y < $rows;
}

# Fonction récursive pour explorer tous les chemins distincts
sub count_trails {
    my ($x, $y, $value, $visited, $path, $unique_paths) = @_;
    
    # Ajoute la position actuelle au chemin
    push @$path, "$x,$y";

    # Marque la position actuelle comme visitée
    $visited->{"$x,$y"} = 1;

    # Indique si cette position est une feuille (fin de chemin)
    my $is_leaf = 1;
    foreach my $dir (@directions) {
        my ($dx, $dy) = @$dir;
        my $nx = $x + $dx;
        my $ny = $y + $dy;

        # Vérifie si le voisin est valide et suit l'ordre attendu
        if (in_bounds($nx, $ny) && !$visited->{"$nx,$ny"} && $grid[$ny][$nx] == $value + 1) {
            $is_leaf = 0; # Cette position n'est pas une feuille
            count_trails($nx, $ny, $value + 1, { %$visited }, [@$path], $unique_paths);
        }
    }

    # Si c'est une feuille, enregistre le chemin comme unique
    if ($is_leaf) {
        my $path_str = join('->', @$path);
        $unique_paths->{$path_str} = 1;
    }
}

# Trouve tous les trailheads (valeur 0) et calcule leurs notes
for (my $i = 0; $i < $rows; $i++) {
    for (my $j = 0; $j < $cols; $j++) {
        if ($grid[$i][$j] == 0) {
            my %unique_paths;
            count_trails($j, $i, 0, {}, [], \%unique_paths);
            $total_rating += scalar keys %unique_paths;
        }
    }
}

# Affiche le score total
print "Total rating of all trailheads: $total_rating\n";
