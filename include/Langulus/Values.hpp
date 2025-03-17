#pragma once


namespace Langulus
{

   ///                                                                        
   /// Can be used to handle value sequences at compile-time                  
   ///                                                                        
   template<auto E1, auto...EN>
   struct Values {
      using FirstType = decltype(E1);
      static constexpr auto First = E1;
      static constexpr bool Empty = false;
      static constexpr ::std::size_t Count = sizeof...(EN) + 1;

      template<unsigned I>
      static consteval auto AtInner() {
         if constexpr (I == 0)
            return E1;
         else if constexpr (I < Count)
            return Values<EN...>::template AtInner<I - 1>();
         else
            static_assert(false, "Index is out of value list bounds");
      }

      template<unsigned I>
      static constexpr auto At = AtInner<I>().First;
   };

} // namespace Langulus
