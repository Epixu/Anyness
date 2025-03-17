#pragma once
#include "../NameOf.hpp"


namespace Langulus
{
   namespace Inner
   {

      template<auto E>
      struct ConstReflector {
         static constexpr Literal Name  = NameOf<E>();
         static constexpr auto    Value = E;
      };

   } // namespace Langulus::Inner


   /// Can be used to reflect named values inside your T like so:             
   /// public: using CTTI_Values = Constants<One, Two, Three>;                
   /// They will be reflected as meta constants on MetaOf<T>                  
   template<auto E1, auto...EN>
   struct Constants {
      using List = Types<
         Inner::ConstReflector<E1>,
         Inner::ConstReflector<EN>...
      >;
   };

} // namespace Langulus
