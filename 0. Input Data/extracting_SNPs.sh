# This is the location of the raw AADR poseidon repository (slightly curated):
POS_DIR=/dir/to/aadr_poseidon

# Prepare the snp file

shuf -n 2000 $POS_DIR/aadr_eig.snp | awk '$2 <= 22' | head -n 1000 > selected_snps.snp
grep rs738409 $POS_DIR/aadr_eig.snp | tail -n1 >> selected_snps.snp
grep rs58542926 $POS_DIR/aadr_eig.snp | tail -n1 >> selected_snps.snp
grep rs641738 $POS_DIR/aadr_eig.snp | tail -n1 >> selected_snps.snp
sort -k2,2n -k4,4n selected_snps.snp > selected_snps_sorted.snp

mkdir -p data


qsub -b y -cwd trident forge -d $POS_DIR --eigenstrat -o data -n aadr_extracted --selectSnps selected_snps_sorted.snp -f "*aadr_poseidon*"

