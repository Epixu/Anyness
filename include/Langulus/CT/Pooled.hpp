#pragma once
#include "../CTTI.hpp"


namespace Langulus
{

   /// Useful for setting CTTI_Pooled                                         
   /// Instructs Fractalloc to pool in the common size-indexed pools          
   ///   @tparam MIN_ALLOC - what's the minimal allocation size in bytes      
   ///   @attention MIN_ALLOC will never be lower than Alignment or the size  
   ///      of the type after reflection. It is always a power-of-two         
   template<unsigned MIN_ALLOC = Alignment>
   struct PooledBySize {
      static constexpr unsigned MinAlloc = MIN_ALLOC;
      static constexpr bool     Enabled  = true;
   };

   /// Useful for setting CTTI_Pooled                                         
   /// Instructs Fractalloc to pool to dedicated type-indexed pools           
   ///   @tparam MIN_ALLOC - what's the minimal allocation size in bytes      
   ///   @tparam MIN_POOL - what's the minimal pool size in bytes             
   ///   @attention MIN_ALLOC will never be lower than Alignment or the size  
   ///      of the type after reflection. It is always a power-of-two         
   ///   @attention MIN_POOL will never be lower than MIN_ALLOC * 256 after   
   ///      reflection. It is always a power-of-two                           
   template<unsigned MIN_ALLOC = Alignment, unsigned MIN_POOL = 1024*1024>
   struct PooledByType {
      static constexpr unsigned MinAlloc = MIN_ALLOC;
      static constexpr unsigned MinPool  = MIN_POOL;
      static constexpr bool     Enabled  = true;
   };

} // namespace Langulus

namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Pooled<T>:                      
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Pooled = PooledBySize/PooledByType;` in T  
   template<class T>
   struct Pooled {
      static constexpr unsigned MinAlloc = Alignment;
      static constexpr unsigned MinPool  = 1024 * 1024;
      static constexpr bool     Enabled  = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Pooled);
