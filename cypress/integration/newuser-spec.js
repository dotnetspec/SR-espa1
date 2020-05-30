
/// <reference types="cypress" />

describe('New User Home Page', () => {
    context('Actions - these tests are NOT currently against the validation text!!!', () => {
        beforeEach(() => {
            //currently have problems if reload for each test
            //best to carry on with the loaded user regist page
            cy.server().route('https://api.jsonbin.io/b/5e66ec74a030db370e1b23fc/latest').as("getUsers");
            cy.server().route('https://api.jsonbin.io/b/5e4cf5f54d073155b0dca915/latest').as("getGlobalRankings");
            cy.visit('/')
            cy.get("@getUsers.2")
            cy.get("@getGlobalRankings.1")
            cy.wait("@getUsers");
            cy.wait("@getGlobalRankings");
        })

        it('successfully loads create new user page', () => {
            cy.get('#registerbtn').click()
            cy.contains('Create New User')
            cy.contains('Please Enter Your User Details And Click \'Register\' below:')
        })

        it('successfully moves to a ranking, selects Join button and displays Create New User', () => {
            cy.contains('SportRank')
            cy.wait(200)
            cy.get('#otherrankingbtn').click()
            cy.contains('SportRank')
            cy.contains('Join?')
            cy.contains('Selected Ranking')
            cy.get('#newUserJoinbtn').wait(200).click()
            cy.contains('Create New User')
        })

        it('successfully validates username', () => {
            //cy.get('#registerbtn').click({ force: true, multiple: true })
            cy.wait(200)
            cy.get('#registerbtn').click()
            //cy.get('[data-top="41"]').click()
            cy.get('#userName')
                .type('jonnahb').should('have.value', 'jonnahb')

                // .type() with special character sequences
                .type('{leftarrow}{rightarrow}{uparrow}{downarrow}')
                .type('{del}{selectall}{backspace}')

                // .type() with key modifiers
                .type('{alt}{option}') //these are equivalent
                .type('{ctrl}{control}') //these are equivalent
                .type('{meta}{command}{cmd}') //these are equivalent
                .type('{shift}')

                // Delay each keypress by 0.1 sec
                .type('jonnahb', { delay: 100 })
                .should('have.value', 'jonnahb')

                //check validates against existing users 
                //nb. currently uses 'real' data from jsonbin (this should be stubbed data)
                .clear()
                .type('Test 10').should('have.value', 'Test 10')
            cy.get('#usernameValidMsg > .s')
            //.should('have.value', 'Username must be unique and between 4-8 characters')
        })

        it('successfully validates description', () => {
            cy.wait(200)
            cy.get('#registerbtn').click()
            cy.get('#userDescription')
                .type('jonnahb').should('have.value', 'jonnahb')

                // .type() with special character sequences
                .type('{leftarrow}{rightarrow}{uparrow}{downarrow}')
                .type('{del}{selectall}{backspace}')

                // .type() with key modifiers
                .type('{alt}{option}') //these are equivalent
                .type('{ctrl}{control}') //these are equivalent
                .type('{meta}{command}{cmd}') //these are equivalent
                .type('{shift}')

                // Delay each keypress by 0.1 sec
                .type('jonnahb', { delay: 100 })
                .should('have.value', 'jonnahb')
        })

        it('successfully validates email', () => {
            cy.wait(200)
            cy.get('#registerbtn').click()
            cy.get('#userEmail')

                .type('fake@email.com').should('have.value', 'fake@email.com')

                // .type() with special character sequences
                .type('{leftarrow}{rightarrow}{uparrow}{downarrow}')
                .type('{del}{selectall}{backspace}')

                // .type() with key modifiers
                .type('{alt}{option}') //these are equivalent
                .type('{ctrl}{control}') //these are equivalent
                .type('{meta}{command}{cmd}') //these are equivalent
                .type('{shift}')

                // Delay each keypress by 0.1 sec
                .type('slow.typing@email.com', { delay: 100 })
                .should('have.value', 'slow.typing@email.com')
        })

        it('successfully validates mobile number', () => {
            cy.wait(200)
            cy.get('#registerbtn').click()
            cy.get('#userMobile')
                .clear()
                .type('1234567890').should('have.value', '1234567890')
                .clear()
                // .type() with special character sequences
                .type('{leftarrow}{rightarrow}{uparrow}{downarrow}')
                .type('{del}{selectall}{backspace}')

                // .type() with key modifiers
                .type('{alt}{option}') //these are equivalent
                .type('{ctrl}{control}') //these are equivalent
                .type('{meta}{command}{cmd}') //these are equivalent
                .type('{shift}')

                // Delay each keypress by 0.1 sec
                .type('1234567890', { delay: 100 })
                .should('have.value', '1234567890')
        })

    })
})