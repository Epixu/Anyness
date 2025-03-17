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

   /// Specialize for all fundamental types                                   
   template<CT::Fundamental T>
   struct Pooled<T> {
      static constexpr unsigned MinAlloc = Alignment;
      static constexpr unsigned MinPool  = 1024 * 1024;
      static constexpr bool     Enabled  = true;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Pooled);

namespace Langulus::CT
{

   template<class T>
   consteval auto GetMinAlloc() {
      if constexpr (requires { CTTI::Pooled<Shed<T>>::Enabled; }) {
         constexpr auto minalloc = CTTI::Pooled<Shed<T>>::MinAlloc;
         return minalloc < Alignment ? Alignment : minalloc;
      }
      else if constexpr (Dense<Shed<T>> and requires { Decay<Shed<T>>::CTTI_Pooled::Enabled; }) {
         constexpr auto minalloc = Decay<Shed<T>>::CTTI_Pooled::MinAlloc;
         return minalloc < Alignment ? Alignment : minalloc;
      }
      else return sizeof(T) < Alignment ? Alignment : sizeof(T);
   }
   
   template<class T>
   consteval auto GetMinPool() {
      if constexpr (requires { CTTI::Pooled<Shed<T>>::Enabled; }) {
         constexpr auto minpool = Roof2(CTTI::Pooled<Shed<T>>::MinPool);
         constexpr auto minallo = Roof2(GetMinAlloc<T>());
         return minpool < minallo ? minallo : minpool;
      }
      else if constexpr (Dense<Shed<T>> and requires { Decay<Shed<T>>::CTTI_Pooled::Enabled; }) {
         constexpr auto minpool = Roof2(Decay<Shed<T>>::CTTI_Pooled::MinPool);
         constexpr auto minallo = Roof2(GetMinAlloc<T>());
         return minpool < minallo ? minallo : minpool;
      }
      else return Roof2(GetMinAlloc<T>() * 256);
   }

} // namespace Langulus::CT
