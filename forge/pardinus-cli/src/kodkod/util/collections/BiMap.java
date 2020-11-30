package kodkod.util.collections;

import java.util.HashMap;
import java.util.Map;

/**
 * Simple bidirectional hashmap implementation.
 * @author Mark Lavrentyev
 */
public class BiMap<K, V>{
    Map<K, V> forwardMap = new HashMap<>();
    Map<V, K> inverseMap = new HashMap<>();

    /**
     * Puts the key-value pair in the bidirectional hashmap.
     */
    public void put(K k, V v) {
        forwardMap.put(k, v);
        inverseMap.put(v, k);
    }

    /**
     * Gets the key associated with the value v.
     * @param v The value to get the key for.
     * @return The key of v.
     */
    public K getK(V v) {
        return inverseMap.get(v);
    }

    /**
     * Gets the value associated with the key k.
     * @param k The key to get the value for.
     * @return The value associated with the key k.
     */
    public V getV(K k) {
        return forwardMap.get(k);
    }

    /**
     * Check if the given key k is in the map.
     */
    public boolean containsK(K k) {
        return forwardMap.containsKey(k);
    }

    /**
     * Check if the given value v is in the map.
     */
    public boolean containsV(V v) {
        return inverseMap.containsKey(v);
    }

    /**
     * Get the number of key-value pairs stored in the map.
     * @return the number of key-value pairs stored in the map.
     */
    public int size() {
        assert forwardMap.size() == inverseMap.size();
        return forwardMap.size();
    }
}
