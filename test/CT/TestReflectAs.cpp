///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/Logger.hpp>
#include "../TestTypes/CommonTypes.hpp"

using namespace Langulus;


///                                                                           
/// CT::Reflectable                                                           
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::Reflectable", TestType
   , ReflectableIntern
   , ReflectableIntern const
   //, ReflectableIntern&     // shouldn't compile at all
   , ReflectableIntern*
   , ReflectableExtern
   , ReflectableExtern const
   //, ReflectableExtern&     // shouldn't compile at all
   , ReflectableExtern*
   , ReflectableAsSelf
   , ReflectableAsSelf const
   //, ReflectableAsSelf&     // shouldn't compile at all
   , ReflectableAsSelf*
   , int
   , int const
   //, int const&, int&       // shouldn't compile at all
   , void*
   , IncompleteType*
) {
   static_assert(CT::Reflectable<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::Reflectable", TestType
   , IncompleteType
   //, IncompleteType&                    // shouldn't compile at all
   , IncompleteType const
   //, IncompleteType const&              // shouldn't compile at all
   //, SheddableType<IncompleteType>      // shouldn't compile at all
   //, SheddableType<IncompleteType>&     // shouldn't compile at all
   //, SheddableType<IncompleteType>*     // shouldn't compile at all
   , void
   , nullptr_t
   , Types<void*>
   //, SheddableType<int>                 // shouldn't compile at all
   //, SheddableType<int*>                // shouldn't compile at all
   //, SheddableType<int* const>          // shouldn't compile at all
   //, SheddableType<int* const&>         // shouldn't compile at all
   , Types<void*>*
   //, SheddableType<int>*                // shouldn't compile at all
   //, SheddableType<int*>*               // shouldn't compile at all
   //, SheddableType<int* const>*         // shouldn't compile at all
   //, SheddableType<int* const&>*        // shouldn't compile at all
   //, SheddableType<NotReflectable>      // shouldn't compile at all
   , NotReflectable
   //, NotReflectable&                    // shouldn't compile at all
   , NotReflectable*
   //, SheddableType<NotReflectableIntern>// shouldn't compile at all
   , NotReflectableIntern
   //, NotReflectableIntern&              // shouldn't compile at all
   , NotReflectableIntern*
   //, SheddableType<NotReflectableExtern>// shouldn't compile at all
   , NotReflectableExtern
   //, NotReflectableExtern&              // shouldn't compile at all
   , NotReflectableExtern*
) {
   static_assert(not CT::Reflectable<TestType>);
}

//static_assert(    CT::Reflectable<>); // shouldn't compile at all
static_assert(    CT::Reflectable<ReflectableIntern, ReflectableExtern, bool>);
static_assert(not CT::Reflectable<ReflectableIntern, ReflectableExtern, NotReflectableExtern>);


#define DEFINE_REFLECTAS_TYPE_TEST(WHAT, RESULT) \
   static_assert(Exact<CT::ReflectedAs<WHAT>, RESULT>);

///                                                                           
/// CT::ReflectedAs                                                           
///                                                                           
SCENARIO("CT::ReflectedAs") {
   DEFINE_REFLECTAS_TYPE_TEST(void,                void)
   DEFINE_REFLECTAS_TYPE_TEST(void*,               void*)
   DEFINE_REFLECTAS_TYPE_TEST(void const*,         void const*)
   DEFINE_REFLECTAS_TYPE_TEST(void const* const,   void const* const)
   DEFINE_REFLECTAS_TYPE_TEST(nullptr_t,           void)
   DEFINE_REFLECTAS_TYPE_TEST(IncompleteType,      void)
   //DEFINE_REFLECTAS_TYPE_TEST(int32_t(&)[5], int32_t)     // shouldn't compile at all
   //DEFINE_REFLECTAS_TYPE_TEST(int32_t[5],    int32_t)     // shouldn't compile at all

   DEFINE_REFLECTAS_TYPE_TEST(int,           int)
   //DEFINE_REFLECTAS_TYPE_TEST(int&,          int)         // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(int const,     int const)
   DEFINE_REFLECTAS_TYPE_TEST(int const*,    int const*)
   DEFINE_REFLECTAS_TYPE_TEST(int,           int)
   //DEFINE_REFLECTAS_TYPE_TEST(int&,          int)         // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(int const,     int const)
   DEFINE_REFLECTAS_TYPE_TEST(int*,          int*)
   DEFINE_REFLECTAS_TYPE_TEST(int const*,    int const*)
   DEFINE_REFLECTAS_TYPE_TEST(int const* const* const,    int const* const* const)
   //DEFINE_REFLECTAS_TYPE_TEST(int const*&&,  int const*)  // shouldn't compile at all

   DEFINE_REFLECTAS_TYPE_TEST(NotReflectable,          void)
   //DEFINE_REFLECTAS_TYPE_TEST(NotReflectable&,         void) // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectable const,    void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectable const*,   void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectable,          void)
   //DEFINE_REFLECTAS_TYPE_TEST(NotReflectable&,         void) // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectable const,    void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectable*,         void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectable const*,   void)
   //DEFINE_REFLECTAS_TYPE_TEST(NotReflectable const*&&, void) // shouldn't compile at all
   
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern,          void)
   //DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern&,         void) // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern const,    void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern const*,   void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern,          void)
   //DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern&,         void) // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern const,    void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern*,         void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern const*,   void)
   //DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern const*&&, void) // shouldn't compile at all
   
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableExtern,          void)
   //DEFINE_REFLECTAS_TYPE_TEST(NotReflectableExtern&,         void) // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableExtern const,    void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableExtern const*,   void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableExtern,          void)
   //DEFINE_REFLECTAS_TYPE_TEST(NotReflectableExtern&,         void) // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableExtern const,    void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableExtern*,         void)
   DEFINE_REFLECTAS_TYPE_TEST(NotReflectableExtern const*,   void)
   //DEFINE_REFLECTAS_TYPE_TEST(NotReflectableIntern const*&&, void) // shouldn't compile at all
   
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern,          char)
   //DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern&,         char)    // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern const,    char const)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern const*,   char const*)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern,          char)
   //DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern&,         char)    // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern const,    char const)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern*,         char*)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern const*,   char const*)
   //DEFINE_REFLECTAS_TYPE_TEST(ReflectableIntern const*&&, char const*&&) // shouldn't compile at all

   DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern,          char)
   //DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern&,         char)       // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern const,    char const)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern const*,   char const*)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern,          char)
   //DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern&,         char)       // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern const,    char const)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern*,         char*)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern const*,   char const*)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern const* const* const,   char const* const* const)
   //DEFINE_REFLECTAS_TYPE_TEST(ReflectableExtern const*&&, char const*&&) // shouldn't compile at all

   DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf,          ReflectableAsSelf)
   //DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf&,         ReflectableAsSelf)         // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf const,    ReflectableAsSelf const)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf const*,   ReflectableAsSelf const*)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf,          ReflectableAsSelf)
   //DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf&,         ReflectableAsSelf)         // shouldn't compile at all
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf const,    ReflectableAsSelf const)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf*,         ReflectableAsSelf*)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf const*,   ReflectableAsSelf const*)
   DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf const* const* const,   ReflectableAsSelf const* const* const)
   //DEFINE_REFLECTAS_TYPE_TEST(ReflectableAsSelf const*&&, ReflectableAsSelf const*&&)// shouldn't compile at all
}
