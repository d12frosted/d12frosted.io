export function groupBy<T, K extends keyof T>(
  data: T[], 
  key: K, 
  keyTransform: (keyValue: T[K]) => string | number = String,
): Record<string | number, T[]> {
  return data.reduce((acc, item) => {
    const groupValue = item[key];
    const groupKey = keyTransform(groupValue);
    
    if (!acc[groupKey]) {
      acc[groupKey] = [];
    }

    acc[groupKey].push(item);

    return acc;
  }, {} as Record<string | number, T[]>);
}

export function mapValues<K extends string | number | symbol, T, U>(
  record: Record<K, T>,
  mapper: (value: T, key: K) => U
): Record<K, U> {
  const result: Record<K, U> = {} as Record<K, U>;
  
  for (const key in record) {
    if (record.hasOwnProperty(key)) {
      result[key] = mapper(record[key], key);
    }
  }

  return result;
}