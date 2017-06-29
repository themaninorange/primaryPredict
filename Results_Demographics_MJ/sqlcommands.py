import pandas as pd
import sys

filenames = raw_input('Enter name of files to re-write -> ')
output_file_name = raw_input('Enter output file name-> ')
files = filenames.split(' ')

columns = list((['number(10)']))
for x in range(0, 4): 
  columns.append('varchar2(30)')
for x in range(0, 100): 
  columns.append('number(2)')


for file in files:
  table_name = file.split('.csv')[0]
  df = pd.read_csv(file)
  num_cols = len(df.columns)
  command = 'CREATE TABLE %s (\n'%table_name
  for i in range(0, num_cols):
    command = command + df.columns[i] + ' ' + columns[i] 
    if i < (num_cols - 1): 
      command = command + ',\n'
  command = command + '\n);\n'

outf = open('output_file_name', 'w')
outf.write(command)


for i in range(0, len(df)):
  command = 'INSERT INTO %s VALUES('%table_name
  for j in range(0, len(df.columns) - 1):
    command = command + str(df.iloc[i,j]) + ','
  command = command[:-1] + ');\n'
  outf.write(command)
outf.close()

